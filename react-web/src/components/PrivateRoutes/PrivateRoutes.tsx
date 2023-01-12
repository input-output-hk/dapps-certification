import React from "react";
import { Outlet, useLocation, Navigate } from "react-router-dom";

import { useAppSelector } from "store/store";

import NotAuthorized from "components/NotAuthorized/NotAuthorized";

import { IUserProfile } from "pages/userProfile/userProfile.interface";

const PrivateRoutes = () => {
  const location = useLocation();
  const renderNoAuthPage = () => {
    if (location.pathname === "/") {
      return <></>;
    } else {
      return <NotAuthorized />;
    }
  };
  const renderOutlets = (userDetails: IUserProfile) => {
    if (location.pathname !== "/profile" && (!userDetails.dapp?.owner || !userDetails.dapp?.repo)) {
      return <Navigate replace to="/profile" />;
    } else {
      return <Outlet />
    }
  }
  const { isLoggedIn, userDetails } = useAppSelector((state) => state.auth);
  return isLoggedIn ? renderOutlets(userDetails) : renderNoAuthPage();
}

export default PrivateRoutes;