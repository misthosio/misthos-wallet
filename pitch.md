# Misthos Pitch
The goal: enable economic activity management in distributed / decentralized / non-hierarchically structured organizations.  This management, to start, specifically consists of compensating contributors to these organizations for 1 off costs, as well as ongoing regular payments in a decentralized manner.  The full vision includes management of an organization’s total financial activity.

## Structure

-The unit of account in Misthos is Bitcoin.
-Every Project has a Bitcoin Wallet secured by a multi-sig address.
-The 'custodians' of the multi-sig are chosen by a pseudo-random process out of all currently active members.
-Payouts that have been approved are batched and submitted to the Bitcoin network at regular intervals.
-The custodians can be rotated every payout by transferring the remaining bitcoin to a new multi-sig address.
-Using Bitcoin assumes the project already has a steady income stream that needs to be distributed fairly to the participants creating the value.
-For projects in early, pre-income phases, tokens may be distributed that can be transferred into Bitcoin once income streams stabilize.

## Projects

-The main container unit of Misthos within which all activity is scoped is called a ‘Project’. Anyone can create a Project.
-Once a Project has been created, members can add other members, submit requests for payouts for 1 off contributions, and agree upon regular payouts to active members.
-Any proposal that a member submits to the system must be reviewed and approved by other members.

## Governance

-How much 'approval' a submission requires to become 'accepted' is determined by numerous situational policies that create a fine grained and well documented governance structure.   
-Policies themselves can also be changed following the same process.
-At Project creation time the 'meta-policy' is determined.  The meta-policy governs how policies can be updated and changed, including the meta-policy itself.
-In order to scale the submission and approval processes to projects with a large number of members, ‘sub-groups’ can be created.
-Sub-groups are implicitly defined via adding 'tags' to submissions and contributors that determine areas of expertise and fitness for reviewing.  This is important to differentiate between decisions effecting a few contributors vs. effecting almost everyone.

## How Misthos Leverages Blockstack

-Blockstack is an absolutely vital part of Misthos.
-All authentication and storage happens via the Blockstack api.
-Ideologically, Misthos wants to transform how organizations can govern their finances in a radically decentralized yet scalable fashion.
-Facilitating non-hierarchical decision making and governance structures is a key component of the post-industrialist future Blockstack is building towards by making data and trust monopolies a thing of the past.
-Without the building blocks Blockstack provides this would currently not be possible without falling back onto relying on centralized services, which would completely defeat the point.

## What Needs to be Done: Now to Signature Berlin

-After a long conceptualization phase and 2 months of dedicated research into the technological and domain specific components in 2017 I am currently working full time on code level implementation.
-My goal for Signature Berlin is to have a working skeleton with the fundamental system architecture in place.  Ideally a demo of a basic end-to-end use case will be possible
-This walk through would include:

- Creating projects
- Adding and approving members.
- Submitting a payout request for a 1-off contribution
- Actually having a payout go through

-The most challenging parts as far as implementation is concerned are:

- An event-log that is synchronised between the individual members blockstack storage
- The bitcoin wallet

-Once these two parts are in place, the addition and subsequent extension of the domain events and logic that support the actual work-flows should be relatively simple.
-My goal by the end of January is to get the end-to-end demo technically working in order to have February for design related activity.

## Beyond Signature Berlin

-Given that I will have the funding to continue work on this project full time I would like to:

- Extend the supported use cases to move towards the vision outlined above so that organizations can manage their entire financial activity in a distributed manner.
- Enhance the implementation to reduce the amount of trust by taking certain threat models into account (e.g. members trying to cheat by maliciously editing their local copies of the event store).
- Extend vision to provide for early stage projects that don't have an income stream and want to distribute tokens as compensation.
- Most of the processes would remain the same but the overall lifecycle of the projects (ie. idea, development, and profit phases) would need to be taken into account.

