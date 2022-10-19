import { fireEvent, getByTestId, render, screen } from "@testing-library/react";
import TimelineItem from "./TimelineItem";

const mockData = {
  status: "queued",
  text: "Queued",
  state: "running",
};

describe("renders the TimelineItem component", () => {
  it("renders component successfully", () => {
    render(<TimelineItem config={mockData} />);
    expect(screen.getByTestId(mockData.status)).toBeInTheDocument();
  });

  it("renders with span for image", () => {
    render(<TimelineItem config={mockData} />);
    const img = screen.getByTestId(mockData.state);
    expect(screen.getByTestId(mockData.status)).toContainElement(img);
  });

  it("renders with span for text", () => {
    render(<TimelineItem config={mockData} />);
    const text = screen.getByTestId(mockData.text);
    expect(screen.getByTestId(mockData.text)).toContainElement(text);
  });
});
