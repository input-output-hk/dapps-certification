import {
    setManyStatus,
    indexOfExecutingProcess,
    processFinishedJson,
  } from "./timeline.helper";
  
  jest.mock("@emurgo/cardano-serialization-lib-browser", () => ({
    BigNum: jest.fn(),
  }));
  
  describe("setManyStatus", () => {
    test("updates state when index is less than the first occurrence of status:: state=outline", () => {
      const config = [
        { status: "running" },
        { status: "pending" },
        { status: "failed" },
      ];
  
      const currentConfig = { state: "outline" };
      const updatedState = "passed";
  
      const updatedConfig = setManyStatus(
        0,
        config,
        currentConfig,
        "failed",
        updatedState
      );
  
      expect(updatedConfig.state).toBe(updatedState);
    });
  
    test("updates state when index is less than the first occurrence of status:: state=running", () => {
      const config = [
        { status: "running" },
        { status: "pending" },
        { status: "failed" },
      ];
  
      const currentConfig = { state: "running" };
      const updatedState = "passed";
  
      const updatedConfig = setManyStatus(
        0,
        config,
        currentConfig,
        "failed",
        updatedState
      );
  
      expect(updatedConfig.state).toBe(updatedState);
    });
  
    test("updates state when index is less than the first occurrence of status:: state=finished", () => {
      const config = [
        { status: "running" },
        { status: "pending" },
        { status: "failed" },
      ];
  
      const currentConfig = { state: "finished" };
      const updatedState = "passed";
  
      const updatedConfig = setManyStatus(
        0,
        config,
        currentConfig,
        "failed",
        updatedState
      );
  
      expect(updatedConfig.state).toBe(currentConfig.state);
    });
  
    test("does not update state when index is greater than or equal to the first occurrence of status", () => {
      const index = 1;
      const config = [
        { status: "queued" },
        { status: "running" },
        { status: "completed" },
      ];
      const currentConfig = { state: "running" };
      const status = "running";
      const updatedState = "updated";
  
      const updatedConfig = setManyStatus(
        index,
        config,
        currentConfig,
        status,
        updatedState
      );
  
      expect(updatedConfig.state).toBe(currentConfig.state);
    });
  });
  
  describe('indexOfExecutingProcess', () => {
    test('returns the index of the first occurrence of the specified state minus one', () => {
      const config = [
        { state: 'running' },
        { state: 'pending' },
        { state: 'completed' },
      ];
      const state = 'pending';
  
      const index = indexOfExecutingProcess(config, state);
  
      expect(index).toBe(0);
    });
  });
  
  
  describe("processFinishedJson", () => {
    test("returns true if all unit tests pass", () => {
      const result = {
        _certRes_DLTests: [
          ["test1", { tag: "Success" }],
          ["test2", { tag: "Success" }],
        ],
        _certRes_unitTestResults: [
          { resultOutcome: { tag: "Success" } },
          { resultOutcome: { tag: "Success" } },
        ],
      };
  
      const isSuccess = processFinishedJson(result);
  
      expect(isSuccess).toBe(true);
    });
  
    test("returns false if any unit test fails", () => {
      const result = {
        _certRes_DLTests: [
          ["test1", { tag: "Success" }],
          ["test2", { tag: "Failure" }],
        ],
        _certRes_unitTestResults: [
          { resultOutcome: { tag: "Success" } },
          { resultOutcome: { tag: "Failure" } },
        ],
      };
  
      const isSuccess = processFinishedJson(result);
  
      expect(isSuccess).toBe(false);
    });
  });