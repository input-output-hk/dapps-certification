import { screen, render } from "@testing-library/react";
import { Form } from "./Form";
import { useForm } from "react-hook-form";
import { Input } from "./components/Input";
import Button from "components/Button/Button";

const Component = () => {
  const form = useForm();
  const onSubmit = jest.fn();

  return (
    <Form form={form} onSubmit={onSubmit}>
      <Input
        label="Twitter"
        type="text"
        id="twitter"
        {...form.register("twitter")}
      />

      <Button type="submit" buttonLabel="Submit form" />
    </Form>
  );
};

describe("Form component", () => {
  it("renders form properly", () => {
    render(<Component />);
    expect(screen.getByText("Twitter")).toBeInTheDocument();
    expect(screen.getByText("Submit form")).toBeInTheDocument();
  });
});