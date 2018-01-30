let text = ReasonReact.stringToElement;

let component = ReasonReact.statelessComponent("Header");

let make = _children => {
  ...component,
  render: (_) =>
    <div>
      <ul className="navbar-nav ml-auto">
        <li className="nav-item" key="0">
          <Router.Link href="/" className="nav-link">
            (text("nav item"))
          </Router.Link>
        </li>
      </ul>
    </div>
};
