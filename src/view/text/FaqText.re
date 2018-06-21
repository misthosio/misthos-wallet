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

let whatAreThePrinciplesBehindMisthosQ = "What are the principles behind Misthos?";
let whatAreThePrinciplesBehindMisthosA =
  <MTypography gutterBottom=true variant=`Body1>
    (
      "Misthos is built on the principles of data sovereignty, identity and collaborative governance models. This "
      |> text
    )
    <a
      href="https://medium.com/@misthosio/ushering-in-the-decentralization-of-work-c32c14e7625c"
      target="_blank">
      ("blog post" |> text)
    </a>
    (" dives into the guiding principles behind Misthos." |> text)
  </MTypography>;

let whatIsBlockstackQ = "Misthos is built on Blockstack. What is Blockstack?";
let whatIsBlockstackA =
  <MTypography gutterBottom=true variant=`Body1>
    (
      "Blockstack is a platform for developing decentralised applications. It provides solution to the decentralization of authentication, authorization and data storage secured by the bitcoin blockchain. With Blockstack, users control their data and apps run on their devices. There are no middlemen, no passwords, no massive data silos to breach, and no services tracking users around the internet. You can learn more about Blockstack "
      |> text
    )
    <a href="https://blockstack.org/" target="_blank"> ("here" |> text) </a>
    ("." |> text)
  </MTypography>;

let doINeedToRegisterWithBlockstackQ = "Do I need to register with Blockstack?";
let doINeedToRegisterWithBlockstackA = "Yes, you need to have a Blockstack ID to sign in to Misthos and create or join a Venture. If you don't have one you will be redirected to the blockstack setup page when signing in.";

let howCanIGetStartedQ = "How can I get started with Misthos?";
let howCanIGetStartedAP1 = "The first step is to set up a Blockstack ID. The Blockstack ID is unique to an individual or a Partner. If you already have a Blockstack ID, then you can directly go to step # 4";
let howCanIGetStartedAP2 =
  <MTypography gutterBottom=true variant=`Body1>
    <ol>
      <li>
        ("Go to " |> text)
        <a href="https://www.misthos.io/" target="_blank">
          ("https://www.misthos.io/" |> text)
        </a>
        ("." |> text)
      </li>
      <li> ("Click on the \"Sign in with Blockstack\" button. " |> text) </li>
      <li>
        (
          "At this point you will be redirected and prompted to install the "
          |> text
        )
        <a href="https://blockstack.org/install" target="_blank">
          ("Blockstack browser" |> text)
        </a>
        (
          " and asked to set up a Blockstack ID. You are now ready to sign-in to Misthos."
          |> text
        )
      </li>
      <li>
        (
          "Start the Blockstack browser on your PC or Laptop. This step is important as the Misthos app calls the Blockstack browser."
          |> text
        )
      </li>
      <li>
        ("Re-open the " |> text)
        <a href="https://www.misthos.io/" target="_blank">
          ("https://www.misthos.io/" |> text)
        </a>
        (
          " page and click on the \"Sign-in with Blockstack\" button. You will see a \"Sign-In Request\" pop up. "
          |> text
        )
      </li>
      <li>
        (
          "Choose the Blockstack ID you want to sign in with and click on \"Approve\" to give Misthos the required permissions."
          |> text
        )
      </li>
    </ol>
    (
      "Once you are successfully signed in, you can either create a new Venture or join an existing one (by invitation)."
      |> text
    )
  </MTypography>;

let whatIsAMisthosVentureQ = "What is a Venture within Misthos?";
let whatIsAMisthosVentureAP1 = "Any project is designated as a Venture within Misthos.";
let whatIsAMisthosVentureAP2 =
  <MTypography gutterBottom=true variant=`Body1>
    ("Every Venture within Misthos has:" |> text)
    <ul>
      <li> ("One or more Partners" |> text) </li>
      <li> ("A multisig BTC wallet at its core" |> text) </li>
    </ul>
  </MTypography>;

let howCreateAVentureQ = "How can I create a Venture within Misthos?";
let howCreateAVentureAP1 =
  <MTypography gutterBottom=true variant=`Body1>
    (
      "To create a Venture, you need to be signed in to Misthos. If you are not yet signed in:"
      |> text
    )
    <ol>
      <li>
        ("Go to " |> text)
        <a href="https://www.misthos.io/" target="_blank">
          ("https://www.misthos.io/" |> text)
        </a>
      </li>
      <li> ("Click on the \"Sign in with Blockstack\" button. " |> text) </li>
      <li>
        (
          "You will see a \"Sign-In Request\" pop up. Choose the blockstack Id you want to sign in with and click on \"Approve\" to give Misthos the required permissions. "
          |> text
        )
      </li>
    </ol>
  </MTypography>;
let howCreateAVentureAP2 = "Once you are signed in to Misthos, you will see the main screen with all the Ventures that you are a part of. To create a new Venture, click on \"Create a Venture\" button.";
let howCreateAVentureAP3 = "You only need to enter a name to create a new Venture. Once created, you will be taken to the main Venture view?. In this view you can see the BTC amount that is held by the wallet of the Venture, the list of Partners, the transaction history and the buttons to receive BTC and create payouts.";

let howJoinVentureQ = "How can I join an existing Venture?";
let howJoinVentureAP1 = "In order to join an existing Venture, a partner within the Venture needs to propose that you be added and share an invitation link. Once all the existing partners within the Venture have endorsed the proposal you will gain access via the link that was shared with you.";
let howJoinVentureAP2 = "If the endorsement process is not yet complete i.e if all existing partners have not yet endorsed a proposed partner, then you will see the following error:";
let howJoinVentureAP3 =
  <MTypography
    variant=`Body2 className=(Css.style([Css.color(Colors.error)]))>
    (
      "Error joining Venture. Perhaps you have not been accepted yet, or if this was your first time logging in to Misthos, the Venture will become available after the inviting partner has logged in again."
      |> text
    )
  </MTypography>;

let whatIsAProposalEndorsementQ = "What is a proposal and an endorsement within Misthos?";
let whatIsAProposalEndorsementAP1 = "Misthos works on a set of proposals and endorsements.";
let whatIsAProposalEndorsementAP2 =
  <MTypography gutterBottom=true variant=`Body1>
    ("Here are the three types of proposals currently available:" |> text)
    <ol>
      <li> ("Propose a Payout" |> text) </li>
      <li> ("Propose adding a Partner" |> text) </li>
      <li> ("Propose removing a Partner" |> text) </li>
    </ol>
  </MTypography>;
let whatIsAProposalEndorsementAP3 = "Each of these proposals require endorsements from the remaining partners to become accepted.";
let whoIsAPartnerQ = "Who is a Partner?";
let whoIsAPartnerA = "A Partner is an individual collaborator within a Venture and is a unique co-signer of the multisig wallet.
";

let howAddRemovePartnerQ = "How do I add or remove Partners within Misthos?";
let howAddRemovePartnerAP1 =
  <MTypography gutterBottom=true variant=`Body1>
    ("In order to add a Partner to a Venture:" |> text)
    <ol>
      <li>
        (
          "Go to the main Venture view and click on \"Add or Remove Partners\"."
          |> text
        )
      </li>
      <li>
        (
          "A pop-up will appear. Here enter the Blockstack ID of the new Partner and click on \"Propose Partner\".
            "
          |> text
        )
      </li>
      <li> ("Share the Venture URL with the added Partner." |> text) </li>
      <li>
        (
          "Access will be granted once all other Partners have endorsed the proposal."
          |> text
        )
      </li>
    </ol>
  </MTypography>;
let howAddRemovePartnerAP2 = "In order to remove a Partner a remove Partner proposal needs to be submitted. This step requires enough existing Partners to endorse the proposal. See also the question on removing a Partner below.";

let howReceiveQ = "How can I receive funds within Misthos?";
let howReceiveA =
  <MTypography gutterBottom=true variant=`Body1>
    ("In order to receive funds:" |> text)
    <ol>
      <li>
        ("Click on the \"Receive\" button on the main home screen." |> text)
      </li>
      <li>
        (
          "A pop-up will appear and create your with your \"Income Address\"."
          |> text
        )
      </li>
      <li>
        (
          "Use this address (either by copying the address or scanning the QR code) to receive funds."
          |> text
        )
      </li>
    </ol>
  </MTypography>;

let howPayoutQ = "How do payouts work within Misthos?";
let howPayoutAP1 = "Any Partner can propose a payout within a Venture. As soon as there is more than one Partner, the funds are collectively controlled and all Partners need to unanimously agree to the payout.";
let howPayoutAP2 =
  <MTypography gutterBottom=true variant=`Body1>
    (
      "The payout process goes through the following steps within Misthos:"
      |> text
    )
    <ol>
      <li> ("Payout Proposal" |> text) </li>
      <li> ("Payout Endorsement or Rejection" |> text) </li>
      <li>
        ("Payout transaction is submitted to the Bitcoin network" |> text)
      </li>
    </ol>
  </MTypography>;
let howPayoutAP3 = "The first two steps involving the proposal and endorsement are intrinsic to Misthos. The third step is extrinsic to Misthos and behaves the same as any transaction submitted to the Bitcoin network.";
