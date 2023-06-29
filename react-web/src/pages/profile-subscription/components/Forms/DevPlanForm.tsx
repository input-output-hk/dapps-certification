import React from "react";
import { Typography } from "@mui/material";
import { useForm } from "hooks/useForm";
import * as yup from "yup";

import { Form } from "compositions/Form/Form";
import { Input } from "compositions/Form/components/Input";
import Button from "components/Button/Button";
import { devPlanSchema } from "../../planValidation.schema";

const DevPlanForm = () => {
  type formDataType = yup.InferType<typeof devPlanSchema>;

  const form: any = useForm({
    schema: devPlanSchema,
    mode: "onChange",
  });

  const formHandler = (formData: formDataType) => {
    console.log(formData);
  };

  return (
    <>
      <div
        style={{ marginBottom: "32px", textAlign: "center" }}
        data-testid="devPlan-form"
      >
        <Typography variant="h5" sx={{ mb: 2 }}>
          Developer Profile
        </Typography>
        <p>Please complete the information to create your account.</p>
      </div>

      <Form
        form={form}
        onSubmit={formHandler}
        style={{ maxWidth: "500px", margin: "auto" }}
      >
        <Input
          label="DApp subject name"
          type="text"
          id="name"
          required={true}
          {...form.register("name")}
        />

        <Input
          label="GitHub Repository"
          type="text"
          id="repo"
          required={true}
          {...form.register("repo")}
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
          id="email"
          required={true}
          {...form.register("email")}
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

export default DevPlanForm;