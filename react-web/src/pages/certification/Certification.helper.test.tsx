import {
    filterCertificationTaskKeys,
    isAnyTaskFailure,
    processTablesDataForChart,
    getPlannedCertificationTaskCount,
  } from "./Certification.helper";
  
  describe("filterCertificationTaskKeys", () => {
    it("should filter certification task keys by type", () => {
      const type = "object";
      const filteredKeys = filterCertificationTaskKeys(type);
      const expectedKeys = [
        "_certRes_standardPropertyResult",
        "_certRes_doubleSatisfactionResult",
        "_certRes_noLockedFundsResult",
        "_certRes_noLockedFundsLightResult",
        "_certRes_standardCrashToleranceResult",
        "_certRes_whitelistResult",
      ];
  
      expect(filteredKeys).toEqual(expectedKeys);
    });
  });
  
  describe("isAnyTaskFailure", () => {
    it("should return true if any certification task has a failure tag", () => {
      const result = {
        _certRes_standardPropertyResult: { tag: "Success" },
        _certRes_doubleSatisfactionResult: { tag: "Failure" },
      };
  
      const hasFailure = isAnyTaskFailure(result);
      expect(hasFailure).toBe(true);
    });
  
    it("should return false if no certification task has a failure tag", () => {
      const result = {
        _certRes_standardPropertyResult: { tag: "Success" },
        _certRes_doubleSatisfactionResult: { tag: "Success" },
      };
  
      const hasFailure = isAnyTaskFailure(result);
      expect(hasFailure).toBe(false);
    });
  });
  
  describe("processTablesDataForChart", () => {
    it("should process tables data for chart", () => {
      const resultObj = {
        output: "Actions (100 in total):",
        tables: {
          Actions: {
            action1: 50,
            action2: 30,
            action3: 20,
          },
        },
      };
      const tableAttr = "Actions";
  
      const processedData = processTablesDataForChart(resultObj, tableAttr);
      const expectedData = [
        ["Actions", "Percentage"],
        ["Action1", 50],
        ["Action2", 30],
        ["Action3", 20],
      ];
      const expectedTotalCount = 100;
  
      expect(processedData.data).toEqual(expectedData);
      expect(processedData.totalCount).toBe(expectedTotalCount);
    });
  
    it("should handle error and return empty data when processing tables data", () => {
      const resultObj = {
        output: "Invalid data",
        tables: {},
      };
      const tableAttr = "InvalidTable";
  
      const processedData = processTablesDataForChart(resultObj, tableAttr);
      const expectedData = [["InvalidTable", "Percentage"]];
      const expectedTotalCount = 0;
  
      expect(processedData.data).toEqual(expectedData);
      expect(processedData.totalCount).toBe(expectedTotalCount);
    });
  });
  
  describe("getPlannedCertificationTaskCount", () => {
    it("should return the count of planned certification tasks", () => {
      const plannedTasks = [
        { name: "Task 1" },
        { name: "Task 2" },
        { name: "Task 3" },
      ];
  
      const taskCount = getPlannedCertificationTaskCount(plannedTasks);
      const expectedCount = 3;
  
      expect(taskCount).toBe(expectedCount);
    });
  
    it("should handle undefined or non-string task names", () => {
      const plannedTasks = [
        { name: "Task 1" },
        { name: undefined },
        { name: 123 },
        { name: null },
        { name: true },
      ];
  
      const taskCount = getPlannedCertificationTaskCount(plannedTasks);
      const expectedCount = 1;
  
      expect(taskCount).toBe(expectedCount);
    });
  });