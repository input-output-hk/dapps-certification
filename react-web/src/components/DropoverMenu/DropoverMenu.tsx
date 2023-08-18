import React from "react";

import Button from "@mui/material/Button";
import Menu from "@mui/material/Menu";

const DropoverMenu = (props?: any) => {
  const [anchorEl, setAnchorEl] = React.useState<null | HTMLElement>(null);
  const open = Boolean(anchorEl);
  const handleClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    setAnchorEl(event.currentTarget);
  };

  return (
    <div className="wrapper">
      <Button
        id={props.name + "-button"}
        aria-controls={open ? "basic-menu" : undefined}
        aria-haspopup="true"
        aria-expanded={open ? "true" : undefined}
        onClick={handleClick}
      >
        {props.mainElm}
      </Button>
      <Menu
        id={props.name + "-menu"}
        anchorEl={anchorEl}
        open={open}
        onClose={(e) => {
          setAnchorEl(null);
        }}
        onClick={()=> { setAnchorEl(null) }}
        MenuListProps={{
          "aria-labelledby": props.name + "-button",
        }}
      >
        {props.listItems}
      </Menu>
    </div>
  );
};

export default DropoverMenu;
