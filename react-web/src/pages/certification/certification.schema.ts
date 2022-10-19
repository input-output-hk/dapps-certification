import { z } from "zod";

export const certificationSchema = z.object({
  username: z.string().min(1, "This field is required"),
  repoName: z.string().min(1, "This field is required"),
  branch: z.string().min(1, "This field is required"),
});
