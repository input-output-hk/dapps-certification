import React from "react";
import { Link } from "react-router-dom";
import "./NotAuthorized.scss";

function NotAuthorized() {
  return (
    <div className="text-wrapper">
      <div className="title" data-content="404">
        403 - ACCESS DENIED
      </div>

      <div className="subtitle">
        Oops, You don't have permission to access this page.
      </div>

      <div className="buttons">
        <Link to="">Go to homepage</Link>
      </div>
    </div>
  );
}

export default NotAuthorized;