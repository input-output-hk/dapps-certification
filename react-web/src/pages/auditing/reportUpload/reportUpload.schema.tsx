import * as yup from "yup";

export const FILE_SIZE = 5;
export const SUPPORTED_FORMATS = [".pdf"];
export const allowedTypes = ["application/pdf"];

export const reportUploadSchema = yup.object().shape({
  certificationLevel: yup.string().required("This field is required"),
  summary: yup
    .string()
    .required("This field is required")
    .max(200, "Please enter less than 200 characters"),
  disclaimer: yup
    .string()
    .required("This field is required")
    .max(200, "Please enter less than 200 characters"),
  subject: yup
    .string()
    .required("This field is required")
    .max(64, "Please enter upto 64 characters")
    .matches(/[0-9a-f]{1,64}/, "Please verify the characters entered"),
  github: yup
    .string()
    .required("This field is required")
    .matches(
      /^(?:https?:\/\/)?(?:www\.)?github\.com\/[\w-]+\/[\w.-]+$/,
      "Please verify the characters entered"
    ),
  name: yup.string().required("This field is required"),
  email: yup
    .string()
    .required("This field is required")
    .matches(/^\S+@\S+\.\S+$/, "Please verify the characters entered"),
  discord: yup
    .string()
    .matches(
      /^(?:https?:\/\/)?discord(?:\.gg|app\.com\/invite|\.com\/invite)\/[\w-]+$/,
      "Please verify the characters entered"
    ),
  logo: yup
    .string()
    .matches(
      /^(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})\.(?:jpg|jpeg|png|gif|bmp|svg|webp|tiff|tif)$/,
      "Please verify the characters entered"
    ),
  twitter: yup
    .string()
    .matches(/@\w{1,15}/, "Please verify the characters entered"),
  website: yup
    .string()
    .required("This field is required")
    .matches(
      /^(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})$/,
      "Please verify the characters entered"
    ),
  // auditReport: yup
  //   .mixed()
  //   .test("required", "This field is required", (value) => {
  //     console.log(value, value.length, !!value.length)
  //     return !!value.length;
  //   })
  //   .test("fileType", "Only PDF is allowed", (value) => {
  //     if (!value[0]) return true; // Skip the test if no file is selected
  //     return Array.from(value).some((file: any) =>
  //       allowedTypes.includes(file.type)
  //     );
  //   }),
  reportURL: yup.string().required("This field is required")
    .matches(/^((^(?!,)|(?!^),\s?)((ipfs:\/\/(Qm[1-9A-HJ-NP-Za-km-z]{44,}|b[A-Za-z2-7]{58,}|B[A-Z2-7]{58,}|z[1-9A-HJ-NP-Za-km-z]{48,}|F[0-9A-F]{50,})([\/?#][-a-zA-Z0-9@:%_+.~#?&\/=]*)*)|((https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})\.(?:json|pdf))))+$/, "Please enter links to JSON/PDF file and/or the ipfs:// link to the report"),
  dAppScripts: yup.array().of(
    yup.object({
      scriptHash: yup
        .string()
        .required("This field is required")
        .matches(/[0-9a-fA-F]{56}/, "Please verify the characters entered"),
      contactAddress: yup.string().required("This field is required"),
      era: yup.string(),
      compiler: yup.string(),
      compilerVersion: yup.string(),
      optimizer: yup.string(),
      optimizerVersion: yup.string(),
      progLang: yup.string(),
      repoUrl: yup.string(),
    })
  ),
});
