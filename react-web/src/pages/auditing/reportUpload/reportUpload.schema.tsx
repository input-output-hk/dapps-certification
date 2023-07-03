import * as yup from "yup";

export const FILE_SIZE = 5;
export const SUPPORTED_FORMATS = [".pdf"];
export const allowedTypes = ["application/pdf"];

export const reportUploadSchema = yup.object().shape({
  certificationLevel: yup.string().required("This field is required"),
  summary: yup
    .string()
    .required("This field is required"),
  disclaimer: yup
    .string()
    .required("This field is required"),
  subject: yup
    .string()
    .required("This field is required")
    .max(64, "Please enter upto 64 characters")
    .matches(/[0-9a-zA-Z_]{1,64}/, "Enter a valid subject name (a-z, A-Z or 0-9 characters only)."),
  name: yup.string().required("This field is required"),
  email: yup
    .string()
    .required("This field is required")
    .matches(/^\S+@\S+\.\S+$/, "Please verify the characters entered"),
  discord: yup
    .string()
    .matches(
      /^(?:https?:\/\/)?discord(?:\.gg|app\.com\/invite|\.com\/invite)\/[\w-]+$/, {
        message: "Please verify the characters entered",
        excludeEmptyString: true 
      }
    ),
  logo: yup
    .string()
    .matches(
      /^(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})\.(?:jpg|jpeg|png|gif|bmp|svg|webp|tiff|tif)$/, {
        message: "Please verify the characters entered",
        excludeEmptyString: true 
      }
    ),
  twitter: yup
    .string().when("twitter", (value) => {
      if (value) {
        return yup.string().matches(/^@\w{1,15}$/, {
          message: "Please verify the characters entered",
          excludeEmptyString: true 
        })
      } else {
        return yup.string().transform((val, originalVal) => !val ? null : originalVal).nullable().optional()
      }
    }),
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
    .matches(/^((^(?!,)|(?!^),\s?)((ipfs:\/\/(Qm[1-9A-HJ-NP-Za-km-z]{44,}|b[A-Za-z2-7]{58,}|B[A-Z2-7]{58,}|z[1-9A-HJ-NP-Za-km-z]{48,}|F[0-9A-F]{50,})([\/?#][-a-zA-Z0-9@:%_+.~#?&\/=]*)*)|((https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})\.(?:json|pdf))))+$/, "Please enter website links to JSON/PDF file and/or the ipfs:// link to the report"),
  dAppScripts: yup.array().of(
    yup.object({
      scriptHash: yup
        .string()
        .required("This field is required")
        .matches(/[0-9a-fA-F]{64}/, "Enter a valid script hash."),
      contactAddress: yup
        .string()
        .matches(/[0-9a-fA-F]{64]/, "Enter a valid contract address."),
      era: yup.string(),
      compiler: yup.string(),
      compilerVersion: yup.string(),
      optimizer: yup.string(),
      optimizerVersion: yup.string(),
      progLang: yup.string(),
      repoUrl: yup.string()
        .matches(
          /^(?:https?:\/\/)?(?:www\.)?github\.com\/[\w-]+\/[\w.-]+$/, {
            message: "Please verify the characters entered",
            excludeEmptyString: true 
          }
        )
    })
  ),
}, [["twitter", "twitter"]]);
