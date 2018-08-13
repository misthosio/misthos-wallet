// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';


function pendingApproval(userId) {
  return "The proposal to add " + (String(userId) + " has to be accepted before onboarding can proceed.");
}

var AlertBox = /* module */[
  /* signInRequired */" has not yet signed into Misthos and therefore\n                        reading their public key (for encrypting data) is\n                        not yet possible. Please remind them to sign in to\n                        automatically expose a public key before joining the\n                        Venture.",
  /* syncRequiredPart1 */" has been accepted and is ready to sync data with\n                      the Venture. Please send them the ",
  /* syncRequiredVentureUrl */"Venture sync URL",
  /* syncRequiredPart2 */" to complete the process.",
  /* pendingApproval */pendingApproval,
  /* fullyOnboarded */" is fully onboarded to this Venture."
];

function signInRequired(userId, localUser, venture, webDomain) {
  return "mailto:?subject=" + (encodeURI("I want to add you to a Misthos Venture: \"" + (String(venture) + "\"")) + ("&body=" + encodeURI("Hi " + (String(userId) + (",\n\nI want to add you to the \"" + (String(venture) + ("\" Venture on Misthos, a multisig bitcoin wallet to collaboratively handle our income and payouts.\n\nBefore I can send you the invite, please sign into " + (String(webDomain) + (" to expose your public key for data encryption (this will happen automatically when you sign in). Let me know when you’ve done so, and I can send you the Venture invite link.\n\nIf you have any questions about Misthos, check out their FAQ (https://www.misthos.io/faq).\n\nThank you,\n" + (String(localUser) + "\n\nwww.misthos.io\n"))))))))));
}

function syncRequired(userId, localUser, venture, joinUrl) {
  return "mailto:?subject=" + (encodeURI("Misthos Venture Invite: " + (String(venture) + "")) + ("&body=" + encodeURI("Hi " + (String(userId) + (",\n\nPlease join the \"" + (String(venture) + ("\" Venture on Misthos, a multisig bitcoin wallet to collaboratively handle our income and payouts. Use the link below to join the Venture:\n\n" + (String(joinUrl) + ("\n\nIf you have any questions about Misthos, check out their FAQ.\n\nThank you,\n" + (String(localUser) + "\n\nwww.misthos.io\n"))))))))));
}

var Email = /* module */[
  /* signInRequired */signInRequired,
  /* syncRequired */syncRequired
];

exports.AlertBox = AlertBox;
exports.Email = Email;
/* No side effect */
