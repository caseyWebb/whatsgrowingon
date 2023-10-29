"use strict";

const { startRegistration } = require("@simplewebauthn/browser");

exports.init = (app) => {
  app.ports.createCredentialPort.subscribe(async (json) => {
    const options = JSON.parse(json);
    console.dir(options);
    const response = await startRegistration(options);
    app.ports.createCredentialResponsePort.send(JSON.stringify(response));
  });
};
