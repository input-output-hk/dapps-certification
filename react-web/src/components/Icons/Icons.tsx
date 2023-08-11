import React from "react";

import {
  Warning,
  Info,
  Error,
  CheckCircle,
  KeyboardArrowDown as Chevron,
  HelpOutlineOutlined,
  AudioFile,
  VideoFile,
  Article,
  PictureAsPdf,
  Delete,
  Image,
  Cancel,
} from "@mui/icons-material";

export enum IconTypes {
  info = "info",
  warning = "warning",
  success = "success",
  error = "error",
  chevron = "chevron",
  question = "question",
  video = "video",
  audio = "audio",
  pdf = "pdf",
  document = "document",
  delete = "delete",
  image = "image",
  close = "close",
}

export interface IconProps {
  className?: string;
  type: string;
  color?: any;
  fontSize?: "inherit" | "large" | "medium" | "small";
  onClick?: (e: any) => any;
}

const Icons = ({
  className = "",
  fontSize = "small",
  type = "info",
  color = "inherit",
  onClick,
}: IconProps) => {
  switch (type) {
    case IconTypes.info:
      return (
        <Info
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.warning:
      return (
        <Warning
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.success:
      return (
        <CheckCircle
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.error:
      return (
        <Error
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.chevron:
      return (
        <Chevron
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.question:
      return (
        <HelpOutlineOutlined
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.audio:
      return (
        <AudioFile
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.video:
      return (
        <VideoFile
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.document:
      return (
        <Article
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.pdf:
      return (
        <PictureAsPdf
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.image:
      return (
        <Image
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.delete:
      return (
        <Delete
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );

    case IconTypes.close:
      return (
        <Cancel
          color={color}
          className={className}
          fontSize={fontSize}
          onClick={onClick}
        />
      );
    default:
      return null;
  }
};

export default Icons;
