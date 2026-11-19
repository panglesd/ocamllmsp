# What are we going to do

## Requests that we can forward to the LLM

- Diagnostics
- Locate
- Rename
- Document
- Completion
- Code action

## How are we going to do

- When should we do the calls to the LLM?
  - Every modification?
  - Or with debouncing?
  - Or manually through some custom trigger?
- What is the input to the LLM
  - Should we send each time all the opened buffers?
  - Or only the one modified?
  - What text prompt exactly are we doing?
- How to read the output of the LLM
  - Structured output I think is possible.
  - Should it generate the json directly, and we don't validate
  - Or should we validate through our format and then create the json through
    the json library?
- What "tools" are we going to offer to the AI?
  - Helpers with position
  - Helpers with making json output?
  - Getting content of opened buffers or even any file in the workspace?
