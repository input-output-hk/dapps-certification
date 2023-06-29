import React from "react";
import { Typography } from "@mui/material";
import { useForm } from "hooks/useForm";
import * as yup from "yup";

import { Form } from "compositions/Form/Form";
import { Input } from "compositions/Form/components/Input";
import Button from "components/Button/Button";
import { auditorPlanSchema } from "../../planValidation.schema";

const AuditorPlanForm = () => {
  type formDataType = yup.InferType<typeof auditorPlanSchema>;

  const form: any = useForm({
    schema: auditorPlanSchema,
    mode: "onChange",
  });

  const formHandler = (formData: formDataType) => {
    console.log(formData);
  };

  return (
    <>
      <div
        style={{ marginBottom: "32px", textAlign: "center" }}
        data-testid="auditorPlan-form"
      >
        <Typography variant="h5" sx={{ mb: 2 }}>
          Auditor Profile
        </Typography>
        <p>Please complete the information to create your account.</p>
      </div>

      <Form
        form={form}
        onSubmit={formHandler}
        style={{ maxWidth: "500px", margin: "auto" }}
      >
        <Input
          label="Company name"
          type="text"
          id="name"
          required={true}
          {...form.register("name")}
        />

        <Input
          label="Email"
          type="text"
          id="email"
          required={true}
          {...form.register("email")}
        />

        <Input
          label="Full name"
          type="text"
          id="fullName"
          required={true}
          {...form.register("fullName")}
        />

        <Input
          label="Contact email"
          type="text"
          id="contactEmail"
          required={true}
          {...form.register("contactEmail")}
        />

        <Input
          label="Website"
          type="text"
          id="website"
          {...form.register("website")}
        />

        <Input
          label="Twitter"
          type="text"
          id="twitter"
          {...form.register("twitter")}
        />

        <Input
          label="LinkedIn"
          type="text"
          id="linkedIn"
          {...form.register("linkedIn")}
        />

        <div className="button-wrapper" style={{ textAlign: "center" }}>
          <Button
            disabled={!form.formState.isValid}
            type="submit"
            buttonLabel={"Pay"}
          />
        </div>
      </Form>
    </>
  );
};

export default AuditorPlanForm;