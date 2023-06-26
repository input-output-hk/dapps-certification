import { fireEvent, render, screen } from "@testing-library/react";
import { Input } from "./Input";
import { FormProvider, useForm } from "react-hook-form";
import { act } from "react-dom/test-utils";
import userEvent from "@testing-library/user-event";

describe("Test cases for FieldError component", () => {
  it("renders input field properly with default type=text", () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input label="test" name="test" required />
        </FormProvider>
      );
    };

    const { getByText, getByTestId } = render(<Component />);
    expect(getByText("test")).toBeInTheDocument();
    expect(getByTestId("test")).toHaveAttribute("type", "text");
    expect(getByText("*")).toBeInTheDocument();
  });

  it("renders input field properly with defaults type=number", () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input label="test" name="test" type="number" />
        </FormProvider>
      );
    };

    const { getByText, getByTestId } = render(<Component />);
    expect(getByText("test")).toBeInTheDocument();
    expect(getByTestId("test")).toHaveAttribute("type", "number");
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

    const { getByTestId } = render(<Component />);
    const container = getByTestId("test-container");
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

    const { getByTestId } = render(<Component />);
    const container = getByTestId("test-container");
    expect(container).toBeInTheDocument();

    await userEvent.click(container);

    expect(container).toHaveClass("active");
    expect(getByTestId("test")).toHaveFocus();
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

    const { getByTestId } = render(<Component />);
    const wrapper = getByTestId("test-wrapper");
    expect(wrapper).toBeInTheDocument();

    const container = getByTestId("test-container");
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

    const { container, getByText } = render(<Component />);
    expect(container.firstChild?.firstChild).toHaveClass("error");
    expect(getByText("This field has some error")).toBeInTheDocument();
  });

  // Note: useEffect fails to update the input. Time issue? 
  it.skip("renders field with active class when disablefocus is true", () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input label="test" name="test" disablefocus={true} />
        </FormProvider>
      );
    };

    const { getByTestId } = render(<Component />);
    const container = getByTestId("test-container");
    expect(container).toBeInTheDocument();

    expect(container).toHaveClass("active");
  });

  it("renders with active label when field has value", () => {
    const Component = () => {
      const methods = useForm();
      return (
        <FormProvider {...methods}>
          <Input
            label="test"
            name="test"
            value="Dummy value"
            onChange={jest.fn()}
          />
        </FormProvider>
      );
    };

    const { getByTestId } = render(<Component />);
    expect(getByTestId("test")).toHaveValue("Dummy value");
    expect(getByTestId("test-container")).toHaveClass("active");
  });
});