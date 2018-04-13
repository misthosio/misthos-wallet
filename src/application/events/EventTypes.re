open PrimitiveTypes;

type proposal('a) = {
  processId,
  dependsOn: option(processId),
  supporterId: userId,
  policy: Policy.t,
  data: 'a,
};

type endorsement = {
  processId,
  supporterId: userId,
};

type acceptance('a) = {
  processId,
  dependsOn: option(processId),
  data: 'a,
};

module type EventData = {
  type t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

module type ProposedEvent =
  (Data: EventData) =>
  {
    type t = proposal(Data.t);
    let make:
      (
        ~dependsOn: option(processId)=?,
        ~supporterId: userId,
        ~policy: Policy.t,
        Data.t
      ) =>
      t;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
  };

let makeProposal = (name: string) : (module ProposedEvent) =>
  (module
   (Data: EventData) => {
     type t = proposal(Data.t);
     let make =
         (~dependsOn: option(processId)=None, ~supporterId, ~policy, data) => {
       processId: ProcessId.make(),
       dependsOn,
       supporterId,
       policy,
       data,
     };
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("dependsOn", nullable(ProcessId.encode, event.dependsOn)),
           ("supporterId", UserId.encode(event.supporterId)),
           ("policy", Policy.encode(event.policy)),
           ("data", Data.encode(event.data)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         dependsOn: raw |> field("dependsOn", optional(ProcessId.decode)),
         supporterId: raw |> field("supporterId", UserId.decode),
         policy: raw |> field("policy", Policy.decode),
         data: raw |> field("data", Data.decode),
       };
   });

module type EndorsedEvent = {
  type t = endorsement;
  let make: (~processId: processId, ~supporterId: userId) => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

let makeEndorsement = (name: string) : (module EndorsedEvent) =>
  (module
   {
     type t = endorsement;
     let make = (~processId, ~supporterId) => {processId, supporterId};
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("supporterId", UserId.encode(event.supporterId)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         supporterId: raw |> field("supporterId", UserId.decode),
       };
   });

module type AcceptedEvent =
  (Data: EventData) =>
  {
    type t = acceptance(Data.t);
    let fromProposal: proposal(Data.t) => t;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
  };

let makeAcceptance = (name: string) : (module AcceptedEvent) =>
  (module
   (Data: EventData) => {
     type t = acceptance(Data.t);
     let fromProposal = ({dependsOn, processId, data}: proposal(Data.t)) => {
       dependsOn,
       processId,
       data,
     };
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("dependsOn", nullable(ProcessId.encode, event.dependsOn)),
           ("data", Data.encode(event.data)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         dependsOn: raw |> field("dependsOn", optional(ProcessId.decode)),
         data: raw |> field("data", Data.decode),
       };
   });

module type Process =
  (Data: EventData) =>
  {
    let processName: string;
    module Proposed: {
      type t = proposal(Data.t);
      let make:
        (
          ~dependsOn: option(processId)=?,
          ~supporterId: userId,
          ~policy: Policy.t,
          Data.t
        ) =>
        t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
    module Endorsed: {
      type t = endorsement;
      let make: (~processId: processId, ~supporterId: userId) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
    module Accepted: {
      type t = acceptance(Data.t);
      let fromProposal: proposal(Data.t) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
  };

let makeProcess = (name: string) : (module Process) =>
  (module
   (Data: EventData) => {
     let processName = name ++ "ApprovalProcess";
     module Proposed = (val makeProposal(name ++ "Proposed"))(Data);
     module Endorsed = (val makeEndorsement(name ++ "Endorsed"));
     module Accepted = (val makeAcceptance(name ++ "Accepted"))(Data);
   });
