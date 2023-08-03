import React, { useState, useEffect, useRef } from "react";
import { UploadProps } from "./interface";
import Icons, { IconTypes } from "components/Icons/Icons";
import HelperText from "components/HelperText/HelperText";

import "./Upload.scss";
import Loader from "components/Loader/Loader";
import ArrowTooltip from "components/Tooltip/Tooltip";
import Button from "components/Button/Button";
import classNames from "classnames";
import { useFormContext } from "react-hook-form";

const Upload: React.FC<UploadProps> = ({
  isMultiple = false,
  name,
  uploadFiles,
  showPreview = true,
  acceptedTypes = "",
  maxFileSize = 10, // 5mb max limit
  uploadedFiles,
  showInfoIcon: showInfo = false,
  highlightText,
  showDefaultError = false,
  isLoading = false,
  isDeleting = false,
  tooltipText = "",
  required = false,
  className = "",
  onClick,
}) => {
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [error, setError] = useState("");
  const inputRef = useRef<HTMLInputElement | null>(null);

  const {
    formState: { errors },
    register,
    clearErrors,
    setValue
  } = useFormContext();

  const handleUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (!e.target.files) return;
    const currFile = e.target.files[0],
      fileList = e.target.files;
    let files: File[] = [];

    // Iterate through file list
    for (let fileIndex = 0; fileIndex < fileList.length; fileIndex++) {
      if (
        selectedFiles
          ?.map((file) => file.name)
          .indexOf(fileList.item(fileIndex)!.name) === -1
      ) {
        files.push(fileList.item(fileIndex)!);
      }
    }

    if (currFile?.size < maxFileSize * 1000000) {
      setError("");
      const selected = isMultiple ? [...selectedFiles, ...files] : [currFile];
      uploadFiles(selected);
    } else {
      setError(`File size should be below ${maxFileSize}MB`);
    }
  };

  const removeItem = async (e: any, idx: number) => {
    e.preventDefault();
    const filteredFiles = selectedFiles?.filter(
      (_currentFile, index) => index !== idx
    );
    !filteredFiles.length && setError("");
    clearErrors(name);
    setValue(name, filteredFiles);
    uploadFiles(filteredFiles);
  };

  const fileSize = (bytes: number, decimals = 2) => {
    if (bytes === 0) return "0 Bytes";
    const k = 1000;
    const sizes = ["Bytes", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return `${parseFloat((bytes / k ** i).toFixed(decimals))} ${sizes[i]}`;
  };

  const fetchFileIcon = (type: string) => {
    switch (type) {
      case "image":
        return <Icons type={IconTypes.image} />;
      case "audio":
        return <Icons type={IconTypes.audio} />;
      case "video":
        return <Icons type={IconTypes.video} />;
      case "pdf":
        return <Icons type={IconTypes.pdf} />;
      default:
        return <Icons type={IconTypes.document} />;
    }
  };

  const fetchPreview = (file: any, idx: number, type = "") => {
    return (
      <div
        className="preview-pdf rounded relative file-select-wrapper width-fill-available"
        key={file.name + String(idx)}
      >
        <div className="flex">
          <div className="rounded text-center file-icon">
            {fetchFileIcon(type.split("/")[0])}
          </div>
          <div className="flex col" style={{ maxWidth: "80%" }}>
            <span className="text-gray-input text-sm truncate">
              {file.name}
            </span>
            <span className="file-size text-gray-info font-semibold">
              {fileSize(file.size)}
            </span>
          </div>
        </div>

        <span
          className="absolute delete-btn vertical-align"
          onClick={(e) => removeItem(e, idx)}
          title="Remove file"
        >
          {isDeleting ? <Loader /> : <Icons type="delete" />}
        </span>
      </div>
    );
  };

  useEffect(() => {
    setSelectedFiles(uploadedFiles || []);
  }, [uploadedFiles]);

  return (
    <div
      className={classNames(
        "file-upload relative input-wrapper",
        className
      )}
    >
      {/* Loader for file upload or delete */}
      {isLoading && (
        <div className="loader-wrapper vertical-align">
          <Loader />
        </div>
      )}

      {/* Click to upload text banner */}
      <div className={classNames("file-upload-text rounded relative outline", {
        error: errors?.[name],
      })}>
        <div className="flex justify-between items-center">
          <div className="text-center">
            {highlightText}
            {required ? <span style={{ color: "red" }}> *</span> : null}
          </div>
          <label htmlFor={name} {...register(name)}>
            <input
              id={name}
              name={name}
              type="file"
              className="hidden"
              multiple={isMultiple}
              accept={acceptedTypes}
              onClick={(event: any) => {
                event.target.value = "";
              }}
              onChange={(e) => {
                clearErrors(name);
                handleUpload(e);
              }}
              disabled={isLoading}
              ref={inputRef}
            />
            <Button
              displayStyle="gradient"
              size="thin"
              buttonLabel="Upload"
              type="button"
              onClick={() => {
                onClick && onClick(selectedFiles);
                inputRef.current?.click();
              }}
            />
          </label>
        </div>

        {tooltipText ? (
          <ArrowTooltip title={tooltipText}>
            <Icons type="question" color="grey" />
          </ArrowTooltip>
        ) : null}
      </div>

      {errors?.[name] && (
        <HelperText
          type="error"
          value={errors[name]?.message as string}
          showInfoIcon={showInfo}
        />
      )}

      {error && showDefaultError && (
        <HelperText type="error" value={error} showInfoIcon={showInfo} />
      )}

      {/* File preview section */}
      {selectedFiles?.length && showPreview ? (
        <div className="flex flex-wrap relative file-preview-wrapper pointer">
          {selectedFiles?.map((file, idx) =>
            fetchPreview(file, idx, file?.type?.toLowerCase())
          )}
        </div>
      ) : null}
    </div>
  );
};

export default Upload;
