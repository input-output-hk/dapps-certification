import React, { memo } from "react";
import { useNavigate } from "react-router-dom";
import { logout } from "store/slices/auth.slice";
import { useAppDispatch } from "store/store";
import MenuItem from "@mui/material/MenuItem";
import Avatar from "@mui/material/Avatar";
import "./AvatarDropDown.scss";
import DropoverMenu from "components/DropoverMenu/DropoverMenu";

const AvatarDropDown = () => {
  const navigate = useNavigate();
  const dispatch = useAppDispatch();
  const logOut = () => {
    dispatch(logout());
  };

  const navigateToProfile = () => {
    navigate("/profile");
  };
  const handleLogoutClose = () => {
    logOut();
    navigate("/");
  };

  const AvatarMainMenu = () => <Avatar alt="Profile Photo" src="/images/avatar.svg" />

  const AvatarSubMenu = () => {
    return (<>
      <MenuItem onClick={navigateToProfile}>Profile</MenuItem>
      <MenuItem onClick={handleLogoutClose}>Logout</MenuItem>
    </>)
  }

  return (
    <DropoverMenu name="avatar" mainElm={<AvatarMainMenu />} listItems={<AvatarSubMenu />} > 
    </DropoverMenu>
  );
};

export default memo(AvatarDropDown);
