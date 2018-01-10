type t = {. receive: Event.t => unit};

module CandidateApproval = {
  let make = _suggestion => {
    /* val suggestion = suggestion; */
    val approval = ref(1);
    pub receive = _event => approval := approval^ + 1
  };
};

let addWatcher = (event, watchers) =>
  Event.(
    switch event {
    | CandidateSuggested(suggestion) => [
        CandidateApproval.make(suggestion),
        ...watchers
      ]
    | _ => watchers
    }
  );
