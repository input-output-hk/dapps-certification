import React from "react";
import { Link } from "react-router-dom";
import "./NotFound.scss";

const NotFound = () => {
  return (
    <div className="text-wrapper">
      <div className="title" data-content="404">
        404 - No page found
      </div>
      <div className="buttons">
        <Link to="/" data-testid="home">Go to homepage</Link>
      </div>
    </div>
  );
};

export default NotFound;