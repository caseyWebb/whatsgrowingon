DEFERRED Let clientExtensionResults be the result of calling credential.getClientExtensionResults().

- Perform CBOR decoding on the attestationObject field of the AuthenticatorAttestationResponse structure to obtain the attestation statement format fmt, the authenticator data authData, and the attestation statement attStmt.

DEFERRED Verify that the values of the client extension outputs in clientExtensionResults and the authenticator extension outputs in the extensions in authData are as expected, considering the client extension input values that were given in options.extensions and any specific policy of the Relying Party regarding unsolicited extensions, i.e., those that were not specified as part of options.extensions. In the general case, the meaning of "are as expected" is specific to the Relying Party and which extensions are in use.

    NOTE: Client platforms MAY enact local policy that sets additional authenticator extensions or client extensions and thus cause values to appear in the authenticator extension outputs or client extension outputs that were not originally specified as part of options.extensions. Relying Parties MUST be prepared to handle such situations, whether it be to ignore the unsolicited extensions or reject the attestation. The Relying Party can make this decision based on local policy and the extensions in use.

    NOTE: Since all extensions are OPTIONAL for both the client and the authenticator, the Relying Party MUST also be prepared to handle cases where none or not all of the requested extensions were acted upon.

    NOTE: The devicePubKey extension has explicit verification procedures, see § 10.2.2.3.1 Registration (create()).

---

Verify that attStmt is a correct attestation statement, conveying a valid attestation signature, by using the attestation statement format fmt’s verification procedure given attStmt, authData and hash.
NOTE: Each attestation statement format specifies its own verification procedure. See § 8 Defined Attestation Statement Formats for the initially-defined formats, and [IANA-WebAuthn-Registries] for the up-to-date list.

If validation is successful, obtain a list of acceptable trust anchors (i.e. attestation root certificates) for that attestation type and attestation statement format fmt, from a trusted source or from policy. For example, the FIDO Metadata Service [FIDOMetadataService] provides one way to obtain such information, using the aaguid in the attestedCredentialData in authData.
Assess the attestation trustworthiness using the outputs of the verification procedure in step 21, as follows:
If no attestation was provided, verify that None attestation is acceptable under Relying Party policy.

If self attestation was used, verify that self attestation is acceptable under Relying Party policy.

Otherwise, use the X.509 certificates returned as the attestation trust path from the verification procedure to verify that the attestation public key either correctly chains up to an acceptable root certificate, or is itself an acceptable certificate (i.e., it and the root certificate obtained in Step 22 may be the same).

Verify that the credentialId is ≤ 1023 bytes. Credential IDs larger than this many bytes SHOULD cause the RP to fail this registration ceremony.

Verify that the credentialId is not yet registered for any user. If the credentialId is already known then the Relying Party SHOULD fail this registration ceremony.

NOTE: The rationale for Relying Parties rejecting duplicate credential IDs is as follows: credential IDs contain sufficient entropy that accidental duplication is very unlikely. However, attestation types other than self attestation do not include a self-signature to explicitly prove possession of the credential private key at registration time. Thus an attacker who has managed to obtain a user’s credential ID and credential public key for a site (this could be potentially accomplished in various ways), could attempt to register a victim’s credential as their own at that site. If the Relying Party accepts this new registration and replaces the victim’s existing credential registration, and the credentials are discoverable, then the victim could be forced to sign into the attacker’s account at their next attempt. Data saved to the site by the victim in that state would then be available to the attacker.

If the attestation statement attStmt verified successfully and is found to be trustworthy, then create and store a new credential record in the user account that was denoted in options.user, with the following contents:
type
credential.type.

id
credential.id or credential.rawId, whichever format is preferred by the Relying Party.

publicKey
The credential public key in authData.

signCount
authData.signCount.

uvInitialized
The value of the UV flag in authData.

transports
The value returned from response.getTransports().

backupEligible
The value of the BE flag in authData.

backupState
The value of the BS flag in authData.

The new credential record MAY also include the following OPTIONAL contents:

attestationObject
response.attestationObject.

attestationClientDataJSON
response.clientDataJSON.

If the attestation statement attStmt successfully verified but is not trustworthy per step 23 above, the Relying Party SHOULD fail the registration ceremony.

NOTE: However, if permitted by policy, the Relying Party MAY register the credential ID and credential public key but treat the credential as one with self attestation (see § 6.5.4 Attestation Types). If doing so, the Relying Party is asserting there is no cryptographic proof that the public key credential has been generated by a particular authenticator model. See [FIDOSecRef] and [UAFProtocol] for a more detailed discussion.

Verification of attestation objects requires that the Relying Party has a trusted method of determining acceptable trust anchors in step 22 above. Also, if certificates are being used, the Relying Party MUST have access to certificate status information for the intermediate CA certificates. The Relying Party MUST also be able to build the attestation certificate chain if the client did not provide this chain in the attestation information
