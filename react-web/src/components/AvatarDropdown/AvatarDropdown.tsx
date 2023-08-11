import React, { memo } from "react";
import { useNavigate } from "react-router-dom";
import { logout } from "store/slices/auth.slice";
import { useAppDispatch } from "store/store";
import MenuItem from "@mui/material/MenuItem";
import Avatar from "@mui/material/Avatar";
import "./AvatarDropDown.scss";
import DropoverMenu from "components/DropoverMenu/DropoverMenu";
import useLocalStorage from "hooks/useLocalStorage";
import { LocalStorageKeys } from 'constants/constants';

const AvatarDropDown = () => {
  const navigate = useNavigate();
  const dispatch = useAppDispatch();
  const logOut = () => {
    dispatch(logout());
  };
  const [, setIsLoggedIn] = useLocalStorage(
    LocalStorageKeys.isLoggedIn,
    localStorage.getItem(LocalStorageKeys.isLoggedIn) === "true" ? true : false
  );

  const [, setUserDetails] = useLocalStorage(
    LocalStorageKeys.userDetails,
    localStorage.getItem(LocalStorageKeys.userDetails)
      ? JSON.parse(localStorage.getItem(LocalStorageKeys.userDetails)!)
      : null
  );

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
      <MenuItem onClick={navigateToProfile}>User Profile</MenuItem>
      <MenuItem onClick={handleLogoutClose}>Logout</MenuItem>
    </>)
  }

  return (
    <DropoverMenu name="avatar" mainElm={<AvatarMainMenu />} listItems={<AvatarSubMenu />} > 
    </DropoverMenu>
  );
};

export default memo(AvatarDropDown);
