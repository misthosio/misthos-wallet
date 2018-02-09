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

module type ProposalEventModuleMaker =
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

let makeProposalEvent = (name: string) : (module ProposalEventModuleMaker) =>
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

module type EndorseEvent = {
  type t = {
    processId,
    supporterId: userId
  };
  let make: (~processId: processId, ~supporterId: userId) => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

let makeEndorseEvent = (name: string) : (module EndorseEvent) =>
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
