include ViewCommon;

let whatIsMisthosQ = "What is Misthos?";
let whatIsMisthosAP1 = "Misthos is a decentralised app for collaborative management of finances. Individuals collaborating on a project can use Misthos to manage and distribute their bitcoin income as a team.";
let whatIsMisthosAP2 = "Users can create Ventures and dynamically add and remove Partners that share and control a multisig Bitcoin wallet. Ventures can receive Bitcoin, and payouts can be proposed that require endorsement from the other Partners before they are submitted to the Bitcoin network.";
let whatIsMisthosAP3 = "In the future Misthos will evolve beyond being a shared wallet to a complete solution for collaboratively managing your cash flow with bitcoin!";

let whoIsMisthosForQ = "Who is Misthos for?";
let whoIsMisthosForA = "Misthos is for any team that need a simple way to transact from a shared multisig Bitcoin wallet.";

let howCanATeamUseMisthosTodayQ = "How can a team use Misthos today?";
let howCanATeamUseMisthosTodayA =
  <MTypography gutterBottom=true variant=`Body1>
    ("Using Misthos, a team can:" |> text)
    <ul>
      <li> ("Aggregate income in a multisig Bitcoin wallet" |> text) </li>
      <li>
        (
          "Dynamically add / remove Partners as they participate in the venture"
          |> text
        )
      </li>
      <li> ("Collectively approve payouts" |> text) </li>
      <li>
        (
          "Have full visibility of the transaction and approval history" |> text
        )
      </li>
    </ul>
  </MTypography>;

let whatIsUniqueAboutMisthosQ = "What is unique about Misthos?";
let whatIsUniqueAboutMisthosAP1 = "The Misthos multisig wallet is optimised for collaborating individuals who need a simple way to collectively share and distribute Bitcoin.";
let whatIsUniqueAboutMisthosAP2 =
  <MTypography gutterBottom=true variant=`Body1>
    ("Misthos is unique for its:" |> text)
    <ol>
      <li> ("Quick wallet set-up" |> text) </li>
      <li>
        (
          "Scalability: dynamically adding / removing wallet custodians (Partners)"
          |> text
        )
      </li>
      <li>
        (
          "Streamlined user-experience for collaborative approval processes"
          |> text
        )
      </li>
    </ol>
  </MTypography>;
