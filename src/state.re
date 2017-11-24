type project = {name: string};

let loadProjects = () => Blockstack.getFile("/index.json", Js.false_);

let initializeProjects = () => Blockstack.putFile("/index.json", "[]", Js.false_);
