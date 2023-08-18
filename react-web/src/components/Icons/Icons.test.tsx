import React from "react";
import { fireEvent, render, screen } from "@testing-library/react";
import Icons, { IconTypes } from "./Icons";

describe("Icons", () => {
  test("renders the correct icon based on the provided type", () => {
    Object.keys(IconTypes).forEach((type) => {
      const { container } = render(<Icons type={type} />);
      expect(container.querySelector(".MuiSvgIcon-root")).toBeInTheDocument();
    });
  });

  test("renders the info icon based when type is not provided", () => {
    render(<Icons />);
    expect(screen.getByTestId("InfoIcon")).toBeInTheDocument();
  });

  test("renders default icon when type is not from mapping", () => {
    render(<Icons type="invalid-type" />);
    expect(screen.queryByTestId(".MuiSvgIcon-root")).not.toBeInTheDocument();
  });

  test("renders icon with provided fontsize", () => {
    render(<Icons fontSize="large" />);
    expect(screen.queryByTestId("InfoIcon")).toHaveClass("MuiSvgIcon-fontSizeLarge");
  });

  test("renders icon with provided color", () => {
    render(<Icons color="grey" />);
    expect(screen.queryByTestId("InfoIcon")).toHaveClass("MuiSvgIcon-colorGrey");
  });

  test("applies the specified className to the icon", () => {
    const className = "custom-icon";
    const { container } = render(
      <Icons type={IconTypes.info} className={className} />
    );
    expect(container.querySelector(`.${className}`)).toBeInTheDocument();
  });

  test("calls the onClick handler when the icon is clicked", () => {
    const onClick = jest.fn();
    const { container } = render(
      <Icons type={IconTypes.info} onClick={onClick} />
    );

    const iconElement = container.querySelector(".MuiSvgIcon-root");
    expect(iconElement).toBeInTheDocument();

    // Simulate a click on the icon
    fireEvent.click(iconElement!);
    expect(onClick).toHaveBeenCalled();
  });
});
