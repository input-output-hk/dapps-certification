import * as yup from "yup";

export const devPlanSchema = yup.object().shape({
  name: yup.string().required("This field is required."),
  repo: yup.string().required("This field is required."),
  fullName: yup.string().required("This field is required."),
  email: yup
    .string()
    .email("Invalid email format")
    .required("This field is required."),
  website: yup.string(),
  twitter: yup.string(),
  linkedIn: yup.string(),
});

export const auditorPlanSchema = yup.object().shape({
  name: yup.string().required("This field is required."),
  email: yup
    .string()
    .email("Invalid email format")
    .required("This field is required."),
  fullName: yup.string().required("This field is required."),
  contactEmail: yup
    .string()
    .email("Invalid email format")
    .required("This field is required."),
  website: yup.string(),
  twitter: yup.string(),
  linkedIn: yup.string(),
});