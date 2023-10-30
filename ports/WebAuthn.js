"use strict";

const {
  startRegistration,
  startAuthentication,
} = require("@simplewebauthn/browser");

exports.init = (app) => {
  app.ports.registerPasskeyPort.subscribe(async (json) => {
    const options = JSON.parse(json);
    const response = await startRegistration(options);
    co;
    app.ports.passkeyRegistrationResponsePort.send({
      username: options.user.name,
      response: JSON.stringify(response),
    });
  });

  app.ports.authenticatePasskeyPort.subscribe(async (json) => {
    const options = JSON.parse(json);
    const response = await startAuthentication(options);
    debugger;
    app.ports.passkeyAuthenticationResponsePort.send({
      response: JSON.stringify(response),
    });
  });
};
