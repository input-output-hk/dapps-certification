import React from "react";
import { render, screen } from "@testing-library/react";
import HelperText, { fetchHelperTextColor } from "./HelperText";

describe("HelperText", () => {
  test("renders the text with correct class name when type=info", () => {
    const value = "This is a info message";
    render(<HelperText value={value} type="info" />);
    const textElement = screen.getByText(value);
    expect(textElement).toHaveClass(fetchHelperTextColor("info"));
  });

  test("renders the text with correct class name when type=warning", () => {
    const value = "This is a warning message";
    render(<HelperText value={value} type="warning" />);
    const textElement = screen.getByText(value);
    expect(textElement).toHaveClass(fetchHelperTextColor("warning"));
  });

  test("renders the text with correct class name when type=success", () => {
    const value = "This is a success message";
    render(<HelperText value={value} type="success" />);
    const textElement = screen.getByText(value);
    expect(textElement).toHaveClass(fetchHelperTextColor("success"));
  });

  test("renders the text with correct class name when type=error", () => {
    const value = "This is a error message";
    render(<HelperText value={value} type="error" />);
    const textElement = screen.getByText(value);
    expect(textElement).toHaveClass(fetchHelperTextColor("error"));
  });

  test("renders the text without an info icon by default and custom class passed", () => {
    const value = "This is a message";
    render(
      <HelperText value={value} className="custom" />
    );
    const iconElement = screen.queryByTestId("InfoIcon");
    expect(iconElement).not.toBeInTheDocument();
    expect(screen.queryByTestId("helper-text-wrapper")).toHaveClass("custom");
  });

  test("renders the text with an info icon when showInfoIcon is true", () => {
    const value = "This is a info message";
    render(
      <HelperText value={value} showInfoIcon={true} />
    );
    const iconElement = screen.queryByTestId("InfoIcon");
    expect(iconElement).toBeInTheDocument();
  });
});