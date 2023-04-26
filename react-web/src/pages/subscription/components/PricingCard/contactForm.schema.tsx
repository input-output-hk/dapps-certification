import * as yup from "yup";

export const contactFormSchema = yup.object().shape({
    name: yup.string().required("This field is required."),
    email: yup.string().email().required("This field is required."),
    subject: yup.string().required("This field is required.")
});
