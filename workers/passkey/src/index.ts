import * as webauthn from "@simplewebauthn/server";
import type {
  VerifiedAuthenticationResponse,
  VerifiedRegistrationResponse,
} from "@simplewebauthn/server";
import type {
  AuthenticationResponseJSON,
  AuthenticatorDevice,
  PublicKeyCredentialCreationOptionsJSON,
  PublicKeyCredentialDescriptorFuture,
  PublicKeyCredentialRequestOptionsJSON,
  RegistrationResponseJSON,
} from "@simplewebauthn/typescript-types";

class MissingParameterError extends Error {
  constructor(param: string) {
    super(`Missing required parameter "${param}"`);
  }
}

class InvalidParameterError extends Error {
  constructor(param: string) {
    super(`Invalid value for parameter "${param}"`);
  }
}

class MalformedRequestBodyError extends Error {
  constructor() {
    super(`Malformed request body`);
  }
}

class VerificationError extends Error {
  constructor() {
    super(`Verification failed`);
  }
}

function identity<T>(value: T): T {
  return value;
}

function isAuthenticatorAttachment(
  value: string | null
): value is AuthenticatorAttachment {
  return value === "platform" || value === "cross-platform";
}

function isCredentialDescriptor<T>(
  value: any
): value is PublicKeyCredentialDescriptorFuture {
  return value.type === "public-key" && typeof value.id === "string";
}

function isRegistrationResponse(value: any): value is RegistrationResponseJSON {
  return (
    typeof value === "object" &&
    value !== null &&
    typeof value.response.clientDataJSON === "string" &&
    typeof value.response.attestationObject === "string"
  );
}

function isAuthenticator(value: any): value is AuthenticatorDevice {
  return (
    typeof value === "object" &&
    value !== null &&
    typeof value.id === "string" &&
    typeof value.counter === "number"
  );
}

function isAuthenticationResponse(
  value: any
): value is AuthenticationResponseJSON {
  return (
    typeof value === "object" &&
    value !== null &&
    typeof value.clientDataJSON === "string" &&
    typeof value.authenticatorData === "string" &&
    typeof value.signature === "string"
  );
}

const NOT_FOUND = Symbol("NOT_FOUND");

export default {
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);
    const params = new URLSearchParams(url.search);

    function getRequiredParam(name: string): string {
      const param = params.get(name);
      if (!param) throw new MissingParameterError(name);
      return param;
    }

    function getOptionalParam<T extends string | null>(
      name: string,
      default_: string | null,
      validator: (param: string | null) => param is T
    ): T {
      const param = params.get(name) || default_;
      if (!validator(param)) throw new InvalidParameterError(name);
      return param;
    }

    function getOptionalParamArray<T extends string>(
      name: string,
      map: (param: string) => T,
      validator: (param: string) => param is T
    ): T[] {
      return params
        .getAll(name)
        .map(map)
        .map((param) => {
          if (!validator(param)) throw new InvalidParameterError(name);
          return param as T;
        });
    }

    async function getBodyParam<T>(
      name: string,
      map: (param: string) => T,
      validator: (param: any) => param is T
    ): Promise<T> {
      const body = await request.json().catch(() => {
        throw new MalformedRequestBodyError();
      });
      if (typeof body !== "object" || body === null) {
        throw new MalformedRequestBodyError();
      }
      if (!(name in body)) throw new MissingParameterError(name);
      const param = map((body as any)[name]);
      if (!validator(param)) throw new InvalidParameterError(name);
      return param;
    }

    async function generateRegistrationOptions(): Promise<PublicKeyCredentialCreationOptionsJSON> {
      return await webauthn.generateRegistrationOptions({
        rpName: getRequiredParam("rpName"),
        rpID: getRequiredParam("rpID"),
        userID: getRequiredParam("userID"),
        userName: getRequiredParam("userName"),
        excludeCredentials: getOptionalParamArray(
          "excludeCredentials[]",
          JSON.parse,
          isCredentialDescriptor
        ),
        authenticatorSelection: {
          authenticatorAttachment: getOptionalParam(
            "authenticatorAttachment",
            "platform",
            isAuthenticatorAttachment
          ),
          residentKey: "required",
          userVerification: "preferred",
        },
        attestationType: "none",
      });
    }

    async function verifyRegistrationResponse(): Promise<{
      counter: number;
      credentialID: string;
      publicKey: string;
    }> {
      const response = await getBodyParam(
        "response",
        JSON.parse,
        isRegistrationResponse
      );
      return await webauthn
        .verifyRegistrationResponse({
          response,
          expectedChallenge: getRequiredParam("challenge"),
          expectedOrigin: getRequiredParam("origin"),
          expectedRPID: getRequiredParam("rpID"),
        })
        .then((res) => {
          if (res.verified) {
            const decoder = new TextDecoder("utf-8");
            return {
              counter: res.registrationInfo!.counter,
              credentialID: decoder.decode(res.registrationInfo!.credentialID),
              publicKey: decoder.decode(
                res.registrationInfo!.credentialPublicKey
              ),
            };
          } else {
            throw new VerificationError();
          }
        });
    }

    async function generateAuthenticationOptions(): Promise<PublicKeyCredentialRequestOptionsJSON> {
      return await webauthn.generateAuthenticationOptions({
        rpID: getRequiredParam("rpID"),
        userVerification: "preferred",
      });
    }

    async function verifyAuthenticationResponse(): Promise<VerifiedAuthenticationResponse> {
      return await webauthn.verifyAuthenticationResponse({
        authenticator: await getBodyParam(
          "authenticator",
          JSON.parse,
          isAuthenticator
        ),
        response: await getBodyParam(
          "response",
          JSON.parse,
          isAuthenticationResponse
        ),
        expectedChallenge: getRequiredParam("challenge"),
        expectedOrigin: getRequiredParam("origin"),
        expectedRPID: getRequiredParam("rpID"),
      });
    }

    const routes: Record<string, Record<string, () => Promise<any>>> = {
      "/register": {
        GET: generateRegistrationOptions,
        POST: verifyRegistrationResponse,
      },
      "/login": {
        GET: generateAuthenticationOptions,
        POST: verifyAuthenticationResponse,
      },
    };

    const response =
      routes[url.pathname]?.[request.method]?.() ?? Promise.resolve(NOT_FOUND);

    const [status, body] = await response
      .then((res) => {
        switch (res) {
          case NOT_FOUND:
            return [404, "Not found"];
          default:
            return [200, res];
        }
      })
      .catch((e) => {
        switch (e.constructor) {
          case MissingParameterError:
          case InvalidParameterError:
          case MalformedRequestBodyError:
          case VerificationError:
            console.log(`Bad request: ${e.message}`);
            return [400, e.message];
          default:
            console.error(`Internal server error: ${e.message}`);
            return [500, e.message];
        }
      });

    return new Response(
      typeof body === "string" ? body : JSON.stringify(body),
      { status }
    );
  },
};
