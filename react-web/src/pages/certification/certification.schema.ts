import * as yup from "yup";

export const certificationSchema = yup.object().shape({
  commit: yup
    .string()
    .required("This field is required.")
    .min(7, "Please enter a commit hash with length atleast 7")
    .max(40, "Please enter a commit hash with length upto 40")
    .matches(
      /[0-9a-f]{7,40}/,
      "Please enter a combination of numbers and lowercase letters through a to f"
    ),
});
