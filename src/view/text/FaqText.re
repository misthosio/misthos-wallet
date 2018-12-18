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
              {"Aggregate income in a multisig Bitcoin wallet" |> text}
            </li>
            <li>
              {
                "Dynamically add / remove Partners as they participate in the venture"
                |> text
              }
            </li>
            <li> {"Collectively approve payouts" |> text} </li>
            <li>
              {
                "Have full visibility of the transaction and approval history"
                |> text
              }
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
            <li> {"Quick wallet set-up" |> text} </li>
            <li>
              {
                "Scalability: dynamically adding / removing wallet custodians (Partners)"
                |> text
              }
            </li>
            <li>
              {
                "Streamlined user-experience for collaborative approval processes"
                |> text
              }
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
            {"blog post" |> text}
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
            {"here" |> text}
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
              {"Go to " |> text}
              <a href="https://www.misthos.io/" target="_blank">
                {"https://www.misthos.io/" |> text}
              </a>
              {"." |> text}
            </li>
            <li>
              {"Click on the \"Sign in with Blockstack\" button. " |> text}
            </li>
            <li>
              {
                "At this point you will be redirected and prompted to install the "
                |> text
              }
              <a href="https://blockstack.org/install" target="_blank">
                {"Blockstack browser" |> text}
              </a>
              {
                " and asked to set up a Blockstack ID. You are now ready to sign-in to Misthos."
                |> text
              }
            </li>
            <li>
              {
                "Start the Blockstack browser on your PC or Laptop. This step is important as the Misthos app calls the Blockstack browser."
                |> text
              }
            </li>
            <li>
              {"Re-open the " |> text}
              <a href="https://www.misthos.io/" target="_blank">
                {"https://www.misthos.io/" |> text}
              </a>
              {
                " page and click on the \"Sign-in with Blockstack\" button. You will see a \"Sign-In Request\" pop up. "
                |> text
              }
            </li>
            <li>
              {
                "Choose the Blockstack ID you want to sign in with and click on \"Approve\" to give Misthos the required permissions."
                |> text
              }
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
            <li> {"One or more Partners" |> text} </li>
            <li> {"A multisig BTC wallet at its core" |> text} </li>
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
              {"Go to " |> text}
              <a href="https://www.misthos.io/" target="_blank">
                {"https://www.misthos.io/" |> text}
              </a>
            </li>
            <li>
              {"Click on the \"Sign in with Blockstack\" button. " |> text}
            </li>
            <li>
              {
                "You will see a \"Sign-In Request\" pop up. Choose the blockstack Id you want to sign in with and click on \"Approve\" to give Misthos the required permissions. "
                |> text
              }
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
        <span className={Css.style([Css.color(Colors.error)])}>
          {
            "Error joining Venture. Perhaps you have not been accepted yet, or if this was your first time logging in to Misthos, the Venture will become available after the inviting partner has logged in again."
            |> text
          }
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
            <li> {"Propose a Payout" |> text} </li>
            <li> {"Propose adding a Partner" |> text} </li>
            <li> {"Propose removing a Partner" |> text} </li>
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
              {
                "Go to the main Venture view and click on \"Add or Remove Partners\"."
                |> text
              }
            </li>
            <li>
              {
                "A pop-up will appear. Here enter the Blockstack ID of the new Partner and click on \"Propose Partner\".
            "
                |> text
              }
            </li>
            <li>
              {"Share the Venture URL with the added Partner." |> text}
            </li>
            <li>
              {
                "Access will be granted once all other Partners have endorsed the proposal."
                |> text
              }
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
              {
                "Click on the \"Receive\" button on the main home screen."
                |> text
              }
            </li>
            <li>
              {
                "A pop-up will appear and create your with your \"Income Address\"."
                |> text
              }
            </li>
            <li>
              {
                {|Use this address (either by copying the address or scanning
                 the QR code) to receive funds.|}
                |> text
              }
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
            <li> {"Payout Proposal" |> text} </li>
            <li> {"Payout Endorsement or Rejection" |> text} </li>
            <li>
              {
                "Payout transaction is submitted to the Bitcoin network" |> text
              }
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
              {
                {|Click on the "Payout" button within the main Venture view.
                   This will open up a "Create a Payout" page.|}
                |> text
              }
            </li>
            <li>
              {"Enter the receiving address and the BTC amount." |> text}
            </li>
            <li>
              {
                {|On the right side of this page, you can see the summary of
                   the payout which includes a breakdown of the BTC amount, the
                   Network fee and Misthos fee.|}
                |> text
              }
            </li>
            <li>
              {
                {|You can now either proceed with this payout by clicking on
                   "Propose Payout" or choose to add another recipient.|}
                |> text
              }
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
    q: "What is the Network fee?",
    a: [|
      S(
        {|The Network fee is the transaction fee that is collected by the miners
         on the Bitcoin network for processing and confirming transactions.
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
  {
    q: "What is the Policy for Payout Endorsements",
    a: [|
      S(
        {|Any Partner within a Venture can propose a payout. For a payout to be
         successfully submitted, it requires N-of-N endorsements i.e all
         Partners within a Venture need to unanimously endorse every payout.|},
      ),
    |],
  },
  {
    q: "What is the Policy for Adding Partners",
    a: [|
      S(
        {|Any Partner within a Venture can propose the addition of a new Partner.
         For the proposal to be successful, it requires N-of-N endorsements i.e
         all Partners within a Venture need to unanimously endorse the proposal.|},
      ),
    |],
  },
  {
    q: "What is the Policy for Removing Partners",
    a: [|
      S(
        {|Any Partner within a Venture can propose the removal of another
         Partner. For the removal to be successful, this proposal requires
         N-1-of-N endorsements. E.g If a Venture has four Partners, then three
         Partners have to endorse the proposed removal.|},
      ),
    |],
  },
  {
    q: "Where can I see the status of a payout?",
    a: [|
      E(
        [|
          {|All payouts are shown in the transactions history and clicking on
          the payout shows its status. A payout goes through the following
          states:|}
          |> text,
          <ul>
            <li>
              <b> {"Pending: " |> text} </b>
              {
                {|These are payouts that are proposed but are still pending some
                 endorsements.|}
                |> text
              }
            </li>
            <li>
              <b> {"Submitted(Unconfirmed): " |> text} </b>
              {
                {|These are payouts that have received all endorsements and
                 submitted to the Bitcoin network.|}
                |> text
              }
            </li>
            <li>
              <b> {"Confirmed:" |> text} </b>
              {{|These payouts are confirmed on the Bitcoin network.|} |> text}
            </li>
          </ul>,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "How long does it take for a payout to be confirmed?",
    a: [|
      S(
        {|Within Misthos, the payout process only takes the time for all
         Partners to endorse the payout. Once all endorsements are received,
         then the payout is submitted as a transaction to the Bitcoin network.
         This is shown as an unconfirmed payout in the transactions history.|},
      ),
      S(
        {|Under normal network conditions, transactions submitted to the Bitcoin
         network by Misthos should be confirmed within the next 60 minutes.
         Due to variance in the time it takes for a block to be mined on the
         Bitcoin blockchain can vary. |},
      ),
      E(
        [|
          "Click " |> text,
          <a
            href="https://bitcoin.org/en/faq#why-do-i-have-to-wait-10-minutes">
            {"here" |> text}
          </a>,
          " for more information" |> text,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "What happens if a Partner rejects a payout?",
    a: [|
      S(
        {|The current endorsement policy requires all Partners to unanimously
         endorse every payout for it to be submitted. If any Partner rejects a
         payout, then the payout is cancelled and will not be submitted.|},
      ),
    |],
  },
  {
    q: "What happens if a Partner leaves without endorsing a payout?",
    a: [|
      S(
        {|If an existing Partner leaves and there are pending payouts within the
         system, then the remaining Partners have to remove the leaving partner.
         Otherwise the pending payouts will be stuck and will never be submitted.|},
      ),
    |],
  },
  {
    q: "Can a payout be cancelled or reversed within Misthos?",
    a: [|
      S(
        {|Yes. Once a payout is proposed by a Partner, any of the remaining
         Partners can chose to reject the payout to cancel it.|},
      ),
      S(
        {|Once all the Partners are received, then the payout is automatically
         submitted to the Bitcoin network and cannot be cancelled or reversed.|},
      ),
    |],
  },
  {
    q: "How can I remove a Partner within Misthos?",
    a: [|
      S(
        {|In order to remove a Partner, then one of the remaining Partners have
         to submit a Partner removal proposal and this proposal needs to be
         endorsed by enough Partners.|},
      ),
      S(
        {|The Partner removal policy requires N-1 of N endorsements. E.g if a
         Venture has four Partners, then in order to remove a Partner, then one
         of remaining three Partners have to propose the removal of the fourth
         Partner and this proposal needs to be endorsed by the other three
         Partners.|},
      ),
    |],
  },
  {
    q: {|How can I leave a Venture within Misthos?
          Can I still have access to a Venture once I leave it?|},
    a: [|
      S(
        {|To leave a Venture, either you or another Partner needs to propose
         your removal. This proposal then needs to be endorsed by the remaining
         Partners for the removal to be successful.|},
      ),
      S(
        {|Once a Partner leaves a Venture, they will only have read-only access
         to the state it was when the Partner left. The read-only access will be
         available via the Venture link.|},
      ),
    |],
  },
  {
    q: "Can I come back to a Venture?",
    a: [|
      S(
        {|Yes, Any of the active Partners can propose and invite you back into
         the Venture.|},
      ),
    |],
  },
  {
    q: "Does Misthos have visibility/ access to my Ventures and my wallet account?",
    a: [|
      S(
        {|No. When you create a Venture within Misthos you connect it to your
         Blockstack ID. By using a Blockstack ID you own the private key and
         control all the data within your Venture. This private key never leaves
         your device and is meant to stay on your laptop/phone. As long as no
         one gets access to your private key, no one has access to your Ventures
         and your wallet account. When you use Blockstack, by design, your
         private keys are never sent to any remote servers.|},
      ),
      E(
        <ul>
          <li>
            {
              {|Misthos is an interface and you are using this interface to
               interact/collaborate with other people on the Blockchain.|}
              |> text
            }
          </li>
          <li>
            {
              {|Misthos does not have access to your private keys and has no
               visibility/access to your Venture and the financial transactions.|}
              |> text
            }
          </li>
        </ul>,
      ),
    |],
  },
  {
    q: "Where is my Venture data stored and how do I control who accesses it?",
    a: [|
      S(
        {|Using the Blockstack ID, you control where your data is stored (you
         could run your own server or use your own cloud storage (Dropbox,
         Amazon S3) and keep backups across all). You then use those places as
         locations pointed to by the URLs in your Blockstack ID's zone file.|},
      ),
      E(
        [|
          "You can find out more details on the Blockstack " |> text,
          <a href="https://blockstack.org/faq"> {"FAQ" |> text} </a>,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
  {
    q: "Can Misthos recover my private key or reverse/ refund my transactions?",
    a: [|
      E(
        [|
          {|No. Misthos cannot:|} |> text,
          <ul>
            <li> {{|Recover or change your private key.|} |> text} </li>
            <li> {{|Recover or reset your Blockstack password.|} |> text} </li>
            <li> {{|Access your Venture or your funds.|} |> text} </li>
            <li> {{|Reverse, cancel, or refund transactions.|} |> text} </li>
            <li> {{|Freeze accounts.|} |> text} </li>
          </ul>,
        |]
        |> ReasonReact.array,
      ),
    |],
  },
|];
