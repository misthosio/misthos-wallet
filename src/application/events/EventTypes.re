open PrimitiveTypes;

type proposal('a) = {
  name: string,
  processId,
  supporterId: userId,
  policy: Policy.t,
  data: 'a
};

type endorsement = {
  name: string,
  processId,
  supporterId: userId
};

type acceptance('a) = {
  name: string,
  processId,
  data: 'a
};

module type EventData = {
  type t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

module type ProposalEvent =
  (Data: EventData) =>
  {
    type t = proposal(Data.t);
    let make: (~supporterId: userId, ~policy: Policy.t, ~data: Data.t) => t;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
  };

let makeProposal = (name: string) : (module ProposalEvent) =>
  (module
   (Data: EventData) => {
     type t = proposal(Data.t);
     let make = (~supporterId, ~policy, ~data) => {
       name,
       processId: ProcessId.make(),
       supporterId,
       policy,
       data
     };
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("supporterId", UserId.encode(event.supporterId)),
           ("policy", Policy.encode(event.policy)),
           ("data", Data.encode(event.data))
         ])
       );
     let decode = raw =>
       Json.Decode.{
         name,
         processId: raw |> field("processId", ProcessId.decode),
         supporterId: raw |> field("supporterId", UserId.decode),
         policy: raw |> field("policy", Policy.decode),
         data: raw |> field("data", Data.decode)
       };
   });

module type EndorsementEvent = {
  type t = endorsement;
  let make: (~processId: processId, ~supporterId: userId) => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

let makeEndorsement = (name: string) : (module EndorsementEvent) =>
  (module
   {
     type t = endorsement;
     let make = (~processId, ~supporterId) => {name, processId, supporterId};
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("supporterId", UserId.encode(event.supporterId))
         ])
       );
     let decode = raw =>
       Json.Decode.{
         name,
         processId: raw |> field("processId", ProcessId.decode),
         supporterId: raw |> field("supporterId", UserId.decode)
       };
   });

module type AcceptanceEvent =
  (Data: EventData) =>
  {
    type t = acceptance(Data.t);
    let make: (~processId: processId, ~data: Data.t) => t;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
  };

let makeAcceptance = (name: string) : (module AcceptanceEvent) =>
  (module
   (Data: EventData) => {
     type t = acceptance(Data.t);
     let make = (~processId, ~data) => {name, processId, data};
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("data", Data.encode(event.data))
         ])
       );
     let decode = raw =>
       Json.Decode.{
         name,
         processId: raw |> field("processId", ProcessId.decode),
         data: raw |> field("data", Data.decode)
       };
   });

module type Process =
  (Data: EventData) =>
  {
    module Proposal: {
      type t = proposal(Data.t);
      let make: (~supporterId: userId, ~policy: Policy.t, ~data: Data.t) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
    module Endorsement: {
      type t = endorsement;
      let make: (~processId: processId, ~supporterId: userId) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
    module Acceptance: {
      type t = acceptance(Data.t);
      let make: (~processId: processId, ~data: Data.t) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
  };

let makeProcess = (name: string) : (module Process) =>
  (module
   (Data: EventData) => {
     module Proposal = (val makeProposal(name ++ "Proposed"))(Data);
     module Endorsement = (val makeEndorsement(name ++ "Endorsed"));
     module Acceptance = (val makeAcceptance(name ++ "Acceptance"))(Data);
   });
