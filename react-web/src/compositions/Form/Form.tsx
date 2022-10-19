import React, { ComponentProps } from "react";

import {
  // context provider for our form
  FormProvider,
  // return type of useHookForm hook
  UseFormReturn,
  // typescript type of form's field values
  FieldValues,
  // type of submit handler event
  SubmitHandler,
} from "react-hook-form";

import classNames from "classnames";
import "./Form.scss";

interface FormProps<T extends FieldValues = any>
  extends Omit<ComponentProps<"form">, "onSubmit"> {
  form: UseFormReturn<T>;
  onSubmit: SubmitHandler<T>;
}

export const Form = <T extends FieldValues>({
  form,
  onSubmit,
  children,
  className,
  ...props
}: FormProps<T>) => {
  return (
    <FormProvider {...form}>
      {/* the `form` passed here is return value of useForm() hook */}
      <form
        autoComplete="off"
        onSubmit={form.handleSubmit(onSubmit)}
        className={classNames("form", className)}
        {...props}
      >
        {/* Disable form modification while submission */}
        <fieldset disabled={form.formState.isSubmitting}>{children}</fieldset>
      </form>
    </FormProvider>
  );
};
