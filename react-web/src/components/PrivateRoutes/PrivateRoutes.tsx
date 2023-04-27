import { Outlet, useLocation, Navigate } from "react-router-dom";

import NotAuthorized from "components/NotAuthorized/NotAuthorized";
import { IUserProfile } from "pages/userProfile/userProfile.interface";

const PrivateRoutes = () => {
  const isLoggedIn = localStorage.getItem("isLoggedIn");
  const userDetailsLS: string | null = localStorage.getItem("userDetails");
  const userDetails: IUserProfile | null = userDetailsLS ? JSON.parse(userDetailsLS) : null;
  const location = useLocation();
  const renderNoAuthPage = () => {
    if (location.pathname === "/") {
      return <></>;
    } else {
      return <NotAuthorized />;
    }
  };
  const renderOutlets = (userDetails: IUserProfile | null) => {
    if (location.pathname !== "/profile" && (!userDetails?.dapp?.owner || !userDetails?.dapp?.repo)) {
      return <Navigate replace to="/profile" />;
    } else {
      return <Outlet />
    }
  }
  return isLoggedIn ? renderOutlets(userDetails) : renderNoAuthPage();
}

export default PrivateRoutes;