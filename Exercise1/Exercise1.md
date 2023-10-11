# Exercise1

A simple end-to-end test of the CSU34016 exercise system.

## Prerequisites

1. Haskell `stack` is installed on the machine you are using.

2. You have also installed `makex1`

3. Your current directory is the one containing this directory (`Exercise1`)

## Task

1. Open a command-line window and navigate to folder **containing** `Exercise1`.
2. Run 'makex1' and supply requested information. 
   This will add file `Exercise1/src/Ex1.hs`.
3. Navigate into `Exercise1`.
4. Enter `stack install`. 
5. If this is your first time running `stack` in one of the CSU34016 Exercise folders, you may have to wait while `stack` ensures it has access to the correct versions of both the Haskell compiler and libraries. This delay should only happen once.
6. Eventually `stack` will compile and build and install the code. A lot of logging "stuff" will scroll past, ending with something like this:

   ```
   Copied executables to /Users/butrfeld/.local/bin:
   - ex1
   ```

7. You can run the program by simply typing `ex1`. 
   Right now this will fail with a message like:

   ```
   Running Exercise1
   ex1: Prelude.undefined
   CallStack (from HasCallStack):
     error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
     undefined, called at src/Ex1.hs:5:9 in main:Ex1
   ```

8. Your task is to edit `src/Ex1.hs` to implement `f1` so it computes the right result.
   When it does, you should see something like:

   ```
   Running Exercise1
   f1(11111111) = 11111153
   ```

9. To submit, simply upload **only** your revised `Ex1.hs` file to Blackboard. 
   Do **not** rename the file in any way.
