[@bs.val] external encodeURI: string => string = "encodeURI";

module AlertBox = {
  let signInRequired = {| has not yet signed into Misthos and therefore
                        reading their public key (for encrypting data) is
                        not yet possible. Please remind them to sign in to
                        automatically expose a public key before joining the
                        Venture.|};
  let syncRequiredPart1 = {| has been accepted and is ready to sync data with
                      the Venture. Please send them the |};
  let syncRequiredVentureUrl = "Venture sync URL";
  let syncRequiredPart2 = " to complete the process.";
  let pendingApproval = (~userId) => {j|The proposal to add $userId has to be accepted before onboarding can proceed.|j};
  let fullyOnboarded = {| is fully onboarded to this Venture.|};
};

module Email = {
  let signInRequired = (~userId, ~localUser, ~venture, ~webDomain) =>
    "mailto:?subject="
    ++ encodeURI({j|I want to add you to a Misthos Venture: "$venture"|j})
    ++ "&body="
    ++ encodeURI(
         {j|Hi $userId,

I want to add you to the "$venture" Venture on Misthos, a multisig bitcoin wallet to collaboratively handle our income and payouts.

Before I can send you the invite, please sign into $webDomain to expose your public key for data encryption (this will happen automatically when you sign in). Let me know when you’ve done so, and I can send you the Venture invite link.

If you have any questions about Misthos, check out their FAQ (https://www.misthos.io/faq).

Thank you,
$localUser

www.misthos.io
|j},
       );

  let syncRequired = (~userId, ~localUser, ~venture, ~joinUrl) =>
    "mailto:?subject="
    ++ encodeURI({j|Misthos Venture Invite: $venture|j})
    ++ "&body="
    ++ encodeURI(
         {j|Hi $userId,

Please join the "$venture" Venture on Misthos, a multisig bitcoin wallet to collaboratively handle our income and payouts. Use the link below to join the Venture:

$joinUrl

If you have any questions about Misthos, check out their FAQ.

Thank you,
$localUser

www.misthos.io
|j},
       );
};
