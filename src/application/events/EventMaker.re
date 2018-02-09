open PrimitiveTypes;

/* module ProposeEvent = { */
/*   type t('a) = { */
/*     processId, */
/*     supporterId: userId, */
/*     data: 'a */
/*   }; */
/* }; */
module type EventData = {
  type t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

module type Proposal =
  (Data: EventData) =>
  {
    type t = {
      processId,
      supporterId: userId,
      policy: Policy.t,
      data: Data.t
    };
    let make: (~supporterId: userId, ~policy: Policy.t, ~data: Data.t) => t;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
  };

let makeProposal = (name: string) : (module Proposal) =>
  (module
   (Data: EventData) => {
     type t = {
       processId,
       supporterId: userId,
       policy: Policy.t,
       data: Data.t
     };
     let make = (~supporterId, ~policy, ~data) => {
       processId: ProcessId.make(),
       supporterId,
       policy,
       data
     };
     let encode = event =>
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
         processId: raw |> field("processId", ProcessId.decode),
         supporterId: raw |> field("supporterId", UserId.decode),
         policy: raw |> field("policy", Policy.decode),
         data: raw |> field("data", Data.decode)
       };
   });

module type Endorsement = {
  type t = {
    processId,
    supporterId: userId
  };
  let make: (~processId: processId, ~supporterId: userId) => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

let makeEndorsement = (name: string) : (module Endorsement) =>
  (module
   {
     type t = {
       processId,
       supporterId: userId
     };
     let make = (~processId, ~supporterId) => {processId, supporterId};
     let encode = event =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("supporterId", UserId.encode(event.supporterId))
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         supporterId: raw |> field("supporterId", UserId.decode)
       };
   });
