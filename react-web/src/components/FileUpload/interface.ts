import { HelperTextType } from "components/HelperText/HelperText";

export interface UploadProps {
  isMultiple: boolean;
  name: string;
  highlightText: string;
  subText?: string;
  uploadFiles: (file: File[]) => void;
  showPreview: boolean;
  acceptedTypes?: string;
  hasPdf?: boolean;
  maxFileSize?: number;
  helpText?: string;
  helpTextType?: HelperTextType;
  uploadedFiles?: any;
  showInfoIcon?: boolean;
  showDefaultError?: boolean; // Error occurs during file upload
  isLoading?: boolean;
  isDeleting?: boolean;
  tooltipText?: string;
  required?: boolean;
  className?: string;
  onClick?: (file: File[]) => void;
}
