import React, { useState } from "react";
import Modal from "components/Modal/Modal";
import parse from 'html-react-parser';

const FileCoverageContainer: React.FC<{
    result: { [x: string]: any };
    githubLink: string;
    coverageFile?: string
}> = ({ result, githubLink, coverageFile = '' }) => {
    const [isOpen, setIsOpen] = useState("")
    const openModal = (file: string) => {
        setIsOpen(file)
    }
    const onCloseModal = (flag: boolean) => {
        setIsOpen("")
    }

    const parseHTMLContents = (filename: string) => {
        const pattern = new RegExp("<h2(.*)>"+filename+"<\/h2>")
        const pre = coverageFile.split(pattern)
        const content = pre[2].split('</pre>')[0] + '</pre>'
        return parse(content);
    }

    const coverageIndexFiles: Array<string> = [];
    const coverageIndexReport: any = {};
    if (result._certRes_coverageReport?._coverageIndex?._coverageMetadata) {
        result._certRes_coverageReport?._coverageIndex?._coverageMetadata.forEach(
            (item: any) => {
                // Find out all files upon which coverage is handled
                const parentLoc =
                    item[0]["tag"] === "CoverLocation"
                        ? item[0]["contents"]
                        : item[0]["contents"][0];
                const covFile: string = parentLoc["_covLocFile"];
                if (covFile && coverageIndexFiles.indexOf(covFile) === -1) {
                    coverageIndexFiles.push(covFile);
                    !coverageIndexReport.hasOwnProperty(covFile) &&
                        (coverageIndexReport[covFile] = []);
                }
                coverageIndexReport.hasOwnProperty(covFile) &&
                    coverageIndexReport[covFile].push(item[0]);
            }
        );
    }

    const coverageFiles: Array<string> = [];
    const coverageReport: any = {};
    if (result._certRes_coverageReport?._coverageData?._coveredAnnotations) {
        result._certRes_coverageReport?._coverageData?._coveredAnnotations.forEach(
            (item: any) => {
                const parentLoc =
                    item["tag"] === "CoverLocation"
                        ? item["contents"]
                        : item["contents"][0];
                const covFile: string = parentLoc["_covLocFile"];
                if (covFile && coverageFiles.indexOf(covFile) === -1) {
                    coverageFiles.push(covFile);
                    !coverageReport.hasOwnProperty(covFile) &&
                        (coverageReport[covFile] = []);
                }
                coverageReport.hasOwnProperty(covFile) &&
                    coverageReport[covFile].push(item[0]);
            }
        );
    }
    
    const percentagePerFile: any = {};
    coverageIndexFiles.forEach(filename => {
        if (coverageReport[filename] && coverageIndexReport[filename]) {
            percentagePerFile[filename] = ((coverageReport[filename].length / coverageIndexReport[filename].length) * 100).toFixed(2)
        }
    })

    const renderRows = () => {
        return coverageIndexFiles ? coverageIndexFiles.map((file: string, index) => {
            return (
                <>
                    <li className="coverage-file">
                        <>
                            {/* To be changed to location of the file code coverage UI */}
                            <span className="link" onClick={(_) => openModal(file)}>{file}</span>
                            <Modal id="coverageHtmlModal" open={isOpen===file} onCloseModal={onCloseModal}>
                                <div>{parseHTMLContents(file)}</div>
                            </Modal>
                        </>
                        <div>
                            <div className="meter-bar">
                                <div className="progress" style={{width: percentagePerFile[file] + "%"}}></div>
                            </div>
                            <span className="coverage-percentage">{percentagePerFile[file]}%</span>
                        </div>
                    </li>
                </>
            );
        }) : null;
    };

    return (
        <div id="coverageIndicator">
            <ul>{renderRows()}</ul>
        </div>
    );
};

export default FileCoverageContainer;
