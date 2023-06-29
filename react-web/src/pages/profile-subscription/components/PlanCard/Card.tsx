import * as React from "react";
import Box from "@mui/material/Box";

import Button from "components/Button/Button";
import "./Card.scss";

export default function Card({
  children,
  buttonText,
  isSelected,
}: {
  children: React.ReactNode;
  buttonText: { baseText: string; selectedText: string };
  isSelected: boolean;
}) {
  return (
    <Box
      sx={{
        width: "100%",
        height: "50vh",
        cursor: "pointer",
        border: "solid",
        borderColor: isSelected ? "#7f5af0" : "lightgrey",
        "&:hover": {
          borderColor: "#7f5af0",
        },
        "&:hover button": {
          backgroundColor: "#7f5af0",
          color: "white",
        },
        textAlign: "center",
        borderRadius: "6px",
        marginBottom: "16px",
        position: "relative",
        userSelect: "none",
        "@media (max-width: 500px)": {
          height: "45vh",
        },
      }}
    >
      <div
        data-testid="card-content"
        style={{ marginBottom: "40px", padding: "20px" }}
      >
        {children}
      </div>
      <Button
        buttonLabel={
          !isSelected ? buttonText.baseText : buttonText.selectedText
        }
        displayStyle={isSelected ? "primary" : "primary-outline"}
        className="card-button"
      ></Button>
    </Box>
  );
}