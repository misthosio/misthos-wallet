include ViewCommon;

type line =
  | S(string)
  | E(ReasonReact.reactElement);

type t = {
  q: string,
  a: array(line),
};

let faq = [|
  {
    q: "What is Misthos?",
    a: [|
      S(
        {|Misthos is a decentralised app for collaborative management of finances.
         Individuals collaborating on a project can use Misthos to manage and
         distribute their bitcoin income as a team.|},
      ),
      S(
        {|Users can create Ventures and dynamically add and remove Partners that
         share and control a multisig Bitcoin wallet. Ventures can receive Bitcoin,
         and payouts can be proposed that require endorsement from the other
         Partners before they are submitted to the Bitcoin network.|},
      ),
      S(
        {|In the future Misthos will evolve beyond being a shared wallet to a
         complete solution for collaboratively managing your cash flow with bitcoin!|},
      ),
    |],
  },
  {
    q: "Who is Misthos for?",
    a: [|
      S(
        {|Misthos is for any team that need a simple way to transact from a shared
       multisig Bitcoin wallet.|},
      ),
    |],
  },
  {
    q: "How can a team use Misthos today?",
    a: [|
      E(
        [|
          "Using Misthos, a team can:" |> text,
          <ul>
            <li>
              ("Aggregate income in a multisig Bitcoin wallet" |> text)
            </li>
            <li>
              (
                "Dynamically add / remove Partners as they participate in the venture"
                |> text
              )
            </li>
            <li> ("Collectively approve payouts" |> text) </li>
            <li>
              (
                "Have full visibility of the transaction and approval history"
                |> text
              )
            </li>
          </ul>,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "What is unique about Misthos?",
    a: [|
      S(
        "The Misthos multisig wallet is optimised for collaborating individuals who need a simple way to collectively share and distribute Bitcoin.",
      ),
      E(
        [|
          "Misthos is unique for its:" |> text,
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
          </ol>,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "What are the principles behind Misthos?",
    a: [|
      E(
        [|
          "Misthos is built on the principles of data sovereignty, identity and collaborative governance models. This "
          |> text,
          <a
            href="https://medium.com/@misthosio/ushering-in-the-decentralization-of-work-c32c14e7625c"
            target="_blank">
            ("blog post" |> text)
          </a>,
          " dives into the guiding principles behind Misthos." |> text,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "Misthos is built on Blockstack. What is Blockstack?",
    a: [|
      E(
        [|
          {|Blockstack is a platform for developing decentralised applications.
          It provides solution to the decentralization of authentication,
          authorization and data storage secured by the bitcoin blockchain. With
          Blockstack, users control their data and apps run on their devices.
          There are no middlemen, no passwords, no massive data silos to breach,
          and no services tracking users around the internet. You can learn more
          about Blockstack|}
          |> text,
          <a href="https://blockstack.org/" target="_blank">
            ("here" |> text)
          </a>,
          "." |> text,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "Do I need to register with Blockstack?",
    a: [|
      S(
        {|Yes, you need to have a Blockstack ID to sign in to Misthos and create
         or join a Venture. If you don't have one you will be redirected to the
         blockstack setup page when signing in.|},
      ),
    |],
  },
  {
    q: "How can I get started with Misthos?",
    a: [|
      S(
        {|The first step is to set up a Blockstack ID. The Blockstack ID is
         unique to an individual or a Partner. If you already have a Blockstack
         ID, then you can directly go to step # 4
         |},
      ),
      E(
        [|
          <ol>
            <li>
              ("Go to " |> text)
              <a href="https://www.misthos.io/" target="_blank">
                ("https://www.misthos.io/" |> text)
              </a>
              ("." |> text)
            </li>
            <li>
              ("Click on the \"Sign in with Blockstack\" button. " |> text)
            </li>
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
          </ol>,
          "Once you are successfully signed in, you can either create a new Venture or join an existing one (by invitation)."
          |> text,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "What is a Venture within Misthos?",
    a: [|
      S("Any project is designated as a Venture within Misthos."),
      E(
        [|
          "Every Venture within Misthos has:" |> text,
          <ul>
            <li> ("One or more Partners" |> text) </li>
            <li> ("A multisig BTC wallet at its core" |> text) </li>
          </ul>,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "How can I create a Venture within Misthos?",
    a: [|
      E(
        [|
          "To create a Venture, you need to be signed in to Misthos. If you are not yet signed in:"
          |> text,
          <ol>
            <li>
              ("Go to " |> text)
              <a href="https://www.misthos.io/" target="_blank">
                ("https://www.misthos.io/" |> text)
              </a>
            </li>
            <li>
              ("Click on the \"Sign in with Blockstack\" button. " |> text)
            </li>
            <li>
              (
                "You will see a \"Sign-In Request\" pop up. Choose the blockstack Id you want to sign in with and click on \"Approve\" to give Misthos the required permissions. "
                |> text
              )
            </li>
          </ol>,
        |]
        |> ReasonReact.array,
      ),
      S(
        {|Once you are signed in to Misthos, you will see the main screen with
         all the Ventures that you are a part of. To create a new Venture, click
         on \"Create a Venture\" button.|},
      ),
      S(
        {|You only need to enter a name to create a new Venture. Once created,
         you will be taken to the main Venture view?. In this view you can see
         the BTC amount that is held by the wallet of the Venture, the list of
         Partners, the transaction history and the buttons to receive BTC and
         create payouts.|},
      ),
    |],
  },
  {
    q: "How can I join an existing Venture?",
    a: [|
      S(
        {|In order to join an existing Venture, a partner within the Venture needs to propose that you be added and share an invitation link. Once all the existing partners within the Venture have endorsed the proposal you will gain access via the link that was shared with you.|},
      ),
      S(
        {|If the endorsement process is not yet complete i.e if all existing partners have not yet endorsed a proposed partner, then you will see the following error:|},
      ),
      E(
        <span className=(Css.style([Css.color(Colors.error)]))>
          (
            "Error joining Venture. Perhaps you have not been accepted yet, or if this was your first time logging in to Misthos, the Venture will become available after the inviting partner has logged in again."
            |> text
          )
        </span>,
      ),
    |],
  },
  {
    q: "What is a proposal and an endorsement within Misthos?",
    a: [|
      S({|Misthos works on a set of proposals and endorsements.|}),
      E(
        [|
          "Here are the three types of proposals currently available:" |> text,
          <ol>
            <li> ("Propose a Payout" |> text) </li>
            <li> ("Propose adding a Partner" |> text) </li>
            <li> ("Propose removing a Partner" |> text) </li>
          </ol>,
        |]
        |> ReasonReact.array,
      ),
      S(
        {|Each of these proposals require endorsements from the remaining
         partners to become accepted.|},
      ),
    |],
  },
  {
    q: "Who is a Partner?",
    a: [|
      S(
        {|A Partner is an individual collaborator within a Venture and is a
         unique co-signer of the multisig wallet.|},
      ),
    |],
  },
  {
    q: "How do I add or remove Partners within Misthos?",
    a: [|
      E(
        [|
          "In order to add a Partner to a Venture:" |> text,
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
            <li>
              ("Share the Venture URL with the added Partner." |> text)
            </li>
            <li>
              (
                "Access will be granted once all other Partners have endorsed the proposal."
                |> text
              )
            </li>
          </ol>,
        |]
        |> ReasonReact.array,
      ),
      S(
        {|In order to remove a Partner a remove Partner proposal needs to be
         submitted. This step requires enough existing Partners to endorse the
         proposal. See also the question on removing a Partner below.|},
      ),
    |],
  },
  {
    q: "How can I receive funds within Misthos?",
    a: [|
      E(
        [|
          "In order to receive funds:" |> text,
          <ol>
            <li>
              (
                "Click on the \"Receive\" button on the main home screen."
                |> text
              )
            </li>
            <li>
              (
                "A pop-up will appear and create your with your \"Income Address\"."
                |> text
              )
            </li>
            <li>
              (
                {|Use this address (either by copying the address or scanning
                 the QR code) to receive funds.|}
                |> text
              )
            </li>
          </ol>,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "How do payouts work within Misthos?",
    a: [|
      S(
        {|Any Partner can propose a payout within a Venture. As soon as there is
         more than one Partner, the funds are collectively controlled and all
         Partners need to unanimously agree to the payout.|},
      ),
      E(
        [|
          "The payout process goes through the following steps within Misthos:"
          |> text,
          <ol>
            <li> ("Payout Proposal" |> text) </li>
            <li> ("Payout Endorsement or Rejection" |> text) </li>
            <li>
              (
                "Payout transaction is submitted to the Bitcoin network" |> text
              )
            </li>
          </ol>,
        |]
        |> ReasonReact.array,
      ),
      S(
        {|The first two steps involving the proposal and endorsement are
         intrinsic to Misthos. The third step is extrinsic to Misthos and
         behaves the same as any transaction submitted to the Bitcoin network.|},
      ),
    |],
  },
  {
    q: "How can I Propose a Payout?",
    a: [|
      E(
        [|
          "In order to propose a payout:" |> text,
          <ol>
            <li>
              (
                {|Click on the "Payout" button within the main Venture view.
                   This will open up a "Create a Payout" page.|}
                |> text
              )
            </li>
            <li>
              ("Enter the receiving address and the BTC amount." |> text)
            </li>
            <li>
              (
                {|On the right side of this page, you can see the summary of
                   the payout which includes a breakdown of the BTC amount, the
                   Network fee and Misthos fee.|}
                |> text
              )
            </li>
            <li>
              (
                {|You can now either proceed with this payout by clicking on
                   "Propose Payout" or choose to add another recipient.|}
                |> text
              )
            </li>
          </ol>,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "How can I Endorse or Reject a Payout?",
    a: [|
      S(
        {|Any proposed payout is added to list of payouts in the main Venture
         view and is visible to all Partners. When a Partner clicks on a
         proposed payout, they can see the overall endorsement status and an
         option to endorse or reject an outstanding payout.
         |},
      ),
      S(
        {|Current policy requires unanimous or N-of-N endorsements. E.g if a
         Venture has three Partners then all three partners need to endorse a
         payout for it to go to the submission stage.
         |},
      ),
      S(
        {|If any partner chooses to reject a payout, then the payout is
         cancelled and will not be submitted.
         |},
      ),
      S(
        {|Once all existing partners endorse the payout, it will be
         automatically submitted to the Bitcoin network. |},
      ),
    |],
  },
  {
    q: "What happens on Payout Submission?",
    a: [|
      S(
        {|Once all endorsements are received then the payout will be submitted
         to the Bitcoin network and will be shown as an “unconfirmed payout” in
         the transactions history on the main Venture page.
         |},
      ),
      S(
        {|Once submitted, a payout behaves the same way as any other transaction
         that is submitted to the Bitcoin network. For a payout to move from
         unconfirmed status to a confirmed status, the transaction needs to be
         added to the Bitcoin blockchain by a miner. Once a payout is confirmed
         on the Bitcoin network, the status will be updated and the funds will
         be transferred.
         |},
      ),
    |],
  },
  {
    q: "What is a Multisig wallet?",
    a: [|
      S(
        {|Multisig, short for Multisignature is a form of technology used to add
         additional security for cryptocurrency transactions. Multisig refers to
         requiring more than one key to authorize a Bitcoin transaction. You can
         read more about Multisig here.
         |},
      ),
      S(
        {|Within Misthos, each Venture has a multisig wallet at its core and
         each Partner has custody of their unique private key. An endorsement
         by a Partner is equivalent to signing with their private key. A
         proposal is successful when enough Partners endorse it i.e sign with
         their private key.
         |},
      ),
    |],
  },
  {
    q: "Who is a custodian of the multisig wallet?",
    a: [|
      S(
        {|A custodian of the multisig wallet is a Partner within a Venture.
         Every Partner has custody of their unique private key.
         |},
      ),
    |],
  },
  {
    q: "Who can see the transactions within a Venture?",
    a: [|
      S(
        {|All existing Partners within a Venture can see the transactions and
         make proposals. Any Partner who has left the Venture has read-only
         access and can see the state at time of leaving.|},
      ),
    |],
  },
  {
    q: "What is the Network fees and Misthos fees?",
    a: [|
      S(
        {|The Network fee is the transaction fee that is collected by the miners
         on the Bitcoin network for processing and confirming transactions.
         |},
      ),
      S(
        {|The Misthos fee is the fee collected by Misthos for using the software.
         The fee is 1.49% of the total amount of a payout
         (disregarding thenetwork fee). Other than usual in crypto exchanges
         there is no fee for income.
         |},
      ),
    |],
  },
  {
    q: "What is a policy in Misthos?",
    a: [|
      S(
        {|A policy specifies the governance rules for how decisions are made
         within Misthos.
         |},
      ),
    |],
  },
  {
    q: {|What policies are there within Misthos?
        Can I change or modify these policies?|},
    a: [|
      S(
        {|There are three default policies specified within Misthos. These are
         hard-coded in the software and users do not have the option to change
         them.|},
      ),
    |],
  },
|];
