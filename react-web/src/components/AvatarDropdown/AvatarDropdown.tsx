import React, { memo } from "react";
import { useNavigate } from "react-router-dom";
import { logout } from "store/slices/auth.slice";
import { useAppDispatch } from "store/store";
import Button from "@mui/material/Button";
import Menu from "@mui/material/Menu";
import MenuItem from "@mui/material/MenuItem";
import Avatar from "@mui/material/Avatar";
import "./AvatarDropDown.scss";

const AvatarDropDown = () => {
  const navigate = useNavigate();
  const dispatch = useAppDispatch();
  const logOut = () => {
    dispatch(logout());
  };

  const [anchorEl, setAnchorEl] = React.useState<null | HTMLElement>(null);
  const open = Boolean(anchorEl);
  const handleClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    setAnchorEl(event.currentTarget);
  };
  const navigateToProfile = () => {
    setAnchorEl(null);
    navigate("/profile");
  };
  const handleLogoutClose = () => {
    setAnchorEl(null);
    logOut();
    navigate("/");
  };

  return (
    <div className="wrapper">
      <Button
        id="avatar-button"
        aria-controls={open ? "basic-menu" : undefined}
        aria-haspopup="true"
        aria-expanded={open ? "true" : undefined}
        onClick={handleClick}
      >
        <Avatar alt="Profile Photo" src="/images/avatar.svg" />
      </Button>
      <Menu
        id="avatar-menu"
        anchorEl={anchorEl}
        open={open}
        onClose={(e) => {
          setAnchorEl(null);
        }}
        MenuListProps={{
          "aria-labelledby": "avatar-button",
        }}
      >
        <MenuItem onClick={navigateToProfile}>User Profile</MenuItem>
        <MenuItem onClick={handleLogoutClose}>Logout</MenuItem>
      </Menu>
    </div>
  );
};

export default memo(AvatarDropDown);
