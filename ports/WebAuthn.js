"use strict";

const { startAuthentication } = require("@simplewebauthn/browser");

exports.init = (app) => {
  app.ports.registerPasskeyPort.subscribe(async (options) => {
    if (typeof window.PublicKeyCredential !== "function") {
      app.ports.passkeyRegistrationResponsePort.send({
        error:
          "Browser does not support passkeys. See https://passkeys.dev/device-support/ for more information",
      });
      return;
    }

    const credential = await navigator.credentials.create({
      publicKey: {
        ...options,
        challenge: base64ToArrayBuffer(options.challenge),
        user: {
          ...options.user,
          id: base64ToArrayBuffer(options.user.id),
        },
        excludeCredentials: options.excludeCredentials.map((c) => ({
          ...c,
          id: base64ToArrayBuffer(c.id),
        })),
      },
    });

    if (!(credential.response instanceof AuthenticatorAttestationResponse)) {
      app.ports.passkeyRegistrationResponsePort.send({
        error: "Invalid response from authenticator",
      });
      return;
    }

    app.ports.passkeyRegistrationResponsePort.send({
      id: credential.id,
      type: credential.type,
      response: {
        attestationObject: arrayBufferToBase64(
          credential.response.attestationObject
        ),
        clientDataJSON: decodeClientDataJSON(
          credential.response.clientDataJSON
        ),
        publicKey: arrayBufferToBase64(credential.response.getPublicKey()),
      },
    });
  });

  app.ports.authenticatePasskeyPort.subscribe(async (json) => {
    app.ports.passkeyAuthenticationResponsePort.send({});
  });
};

function base64ToArrayBuffer(str) {
  str = str.padEnd(str.length + ((4 - (str.length % 4)) % 4), "=");
  const binary = atob(str);
  const buffer = new ArrayBuffer(binary.length);
  const bytes = new Uint8Array(buffer);
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
  return buffer;
}

function arrayBufferToBase64(buffer) {
  const bytes = new Uint8Array(buffer);
  let binary = "";
  for (let i = 0; i < bytes.byteLength; i++)
    binary += String.fromCharCode(bytes[i]);
  return btoa(binary);
}

function decodeClientDataJSON(clientDataJSON) {
  return atob(arrayBufferToBase64(clientDataJSON));
}
