[@bs.val] external encodeURI : string => string = "";

module AlertBox = {
  let signInRequired = {| has not yet signed into Misthos and is therefore
                        missing a public key. Please remind them to sign in to
                        automatically expose a public key before joining the
                        Venture.|};
  let syncRequired = {| has been fully endorsed and is ready to sync data with
                      the Venture. Please send them the Venture sync URL to
                      complete the process.|};
  let pendingApproval = {| has to be accepted before onboarding can proceed.|};
  let fullyOnboarded = {| is fully onboarded to this Venture.|};
};

module Email = {
  let signInRequired = (~userId, ~venture, ~appDomain) =>
    "mailto:?subject="
    ++ encodeURI({j|I want to add you to a Misthos Venture: $venture|j})
    ++ "&body="
    ++ encodeURI(
         {j|Hi $userId,

I want to add you to the $venture Venture on Misthos, a multisig bitcoin wallet to collaboratively handle our income and payouts.

Before I can send you the invite, please sign into $appDomain to create your public key. Let me know when you’ve done so, and I can send you the Venture invite link.

If you have any questions about Misthos, check out their FAQ.

Thanks!
|j},
       );

  let syncRequired = (~userId, ~venture, ~joinUrl) =>
    "mailto:?subject="
    ++ encodeURI({j|Misthos Venture Invite: $venture|j})
    ++ "&body="
    ++ encodeURI(
         {j|Hi $userId,

Please join the $venture Venture on Misthos, a multisig bitcoin wallet to collaboratively handle our income and payouts. Use the link below to join the Venture:

$joinUrl

If you have any questions about Misthos, check out their FAQ.

Thanks!

|j},
       );
};
