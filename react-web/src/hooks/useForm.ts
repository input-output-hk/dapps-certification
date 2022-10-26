import {
  useForm as useHookForm
} from "react-hook-form";

// Form validator library: yup
import { yupResolver } from "@hookform/resolvers/yup";

export const useForm = ({ schema, ...formConfig }: any) => {
  return useHookForm({
    ...formConfig,
    resolver: yupResolver(schema),
  });
};
