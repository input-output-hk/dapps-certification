import { fireEvent, render, screen } from "@testing-library/react";
import Modal from "./Modal";

describe("Modal component", () => {
  it("renders modal component with title and content", () => {
    const onCloseModal = jest.fn();
    const title = "Test Modal Title";
    const content = "Test Modal Content";

    render(
      <Modal open={true} onCloseModal={onCloseModal} title={title}>
        {content}
      </Modal>
    );

    const modalTitle = screen.getByText(title);
    expect(modalTitle).toBeInTheDocument();

    const modalContent = screen.getByText(content);
    expect(modalContent).toBeInTheDocument();
  });

  it("calls onCloseModal when close button is clicked", () => {
    const onCloseModal = jest.fn();
    const title = "Test Modal Title";
    const content = "Test Modal Content";

    render(
      <Modal open={true} onCloseModal={onCloseModal} title={title}>
        {content}
      </Modal>
    );

    const closeButton = screen.getByLabelText("close");
    fireEvent.click(closeButton);

    expect(onCloseModal).toHaveBeenCalled();
  });
});