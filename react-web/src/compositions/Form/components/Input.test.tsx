import { fireEvent, render, screen } from "@testing-library/react";
import { Input } from "./Input";
import { FormProvider, useForm } from "react-hook-form";
import { act } from "react-dom/test-utils";

describe("Test cases for FieldError component", () => {
  it("renders input field properly", () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input label="test" name="test" required />
        </FormProvider>
      );
    };

    render(<Component />);
    expect(screen.getByText("test")).toBeInTheDocument();
    expect(screen.getByText("*")).toBeInTheDocument();
  });

  it("renders disabled input field properly", () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input label="test" name="test" disabled />
        </FormProvider>
      );
    };

    render(<Component />);
    const container = screen.getByTestId("test-container");
    expect(container).toBeInTheDocument();
    expect(container).toHaveClass("disabled");
  });

  it("should be active when field is clicked/focused", async () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input label="test" name="test" />
        </FormProvider>
      );
    };

    const { container: component } = render(<Component />);
    const container = screen.getByTestId("test-container");
    expect(container).toBeInTheDocument();

    await act(() => {
      fireEvent.click(container);
    });

    expect(container).toHaveClass("active");
    expect(screen.getByTestId("test")).toHaveFocus();
  });

  it("should not be active when clicked outside the field", async () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input label="test" name="test" />
        </FormProvider>
      );
    };

    render(<Component />);
    const wrapper = screen.getByTestId("test-wrapper");
    expect(wrapper).toBeInTheDocument();

    const container = screen.getByTestId("test-container");
    expect(container).toBeInTheDocument();

    await act(() => {
      fireEvent.blur(wrapper);
    });
    expect(container).not.toHaveClass("active");
  });

  it("renders input field with custom class name", () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input label="test" name="test" className="test-class" />
        </FormProvider>
      );
    };

    const { container } = render(<Component />);
    expect(container.firstChild).toHaveClass("test-class");
  });

  it("renders field error with custom message", () => {
    const Component = () => {
      const methods = useForm();
      const customMethods = {
        ...methods,
        formState: {
          ...methods.formState,
          errors: {
            test: {
              type: "custom",
              message: "This field has some error",
            },
          },
        },
      };

      return (
        <FormProvider {...customMethods}>
          <Input label="test" name="test" className="test-class" />
        </FormProvider>
      );
    };

    const { container } = render(<Component />);
    expect(container.firstChild?.firstChild).toHaveClass("error");
    expect(screen.getByText("This field has some error")).toBeInTheDocument();
  });
});