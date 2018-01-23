[@bs.module] external logo : string = "../assets/img/reactlogo.png";

[@bs.module] external imagine : string = "../assets/img/sidebar-3.jpg";

let text = ReasonReact.stringToElement;

let component = ReasonReact.statelessComponent("Sidebar");

let make = (~currentUrl, _children) => {
  ...component,
  render: (_) => {
    let sidebarBackground =
      ReactDOMRe.Style.make(~backgroundImage="url(" ++ imagine ++ ")", ());
    <div id="sidebar" className="sidebar">
      <div className="sidebar-background" style=sidebarBackground />
      <div className="logo">
        <a
          href="/"
          className="simple-text logo-mini">
          <div className="logo-img"> <img src=logo alt="logo_image" /> </div>
        </a>
        <a
          href="/"
          className="simple-text logo-normal">
          (text("Misthos"))
        </a>
      </div>
      <div className="sidebar-wrapper">
        <ul className="nav flex-column">
          /*     { this.state.width <= 991 ? (<HeaderLinks />):null } */

            <li className="nav-item" key="0">
              <Router.Link className="nav-link" href="/dashboard">
                <i className="pe-7s-graph" />
                <p> (text("Dashboard")) </p>
              </Router.Link>
            </li>
            <li className="nav-item" key="1">
              <Router.Link className="nav-link" href="/user">
                <i className="pe-7s-user" />
                <p> (text("User Profile")) </p>
              </Router.Link>
            </li>
          </ul>
      </div>
    </div>;
  }
};
