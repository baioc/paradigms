
<h1><div align="center">Managing Accelerated Application Memory with CUDA C/C++ Unified Memory and nvprof</div></h1>

![CUDA](./images/CUDA_Logo.jpg)

The [*CUDA Best Practices Guide*](http://docs.nvidia.com/cuda/cuda-c-best-practices-guide/index.html#memory-optimizations), a highly recommended followup to this and other CUDA fundamentals labs, recommends a design cycle called **APOD**: **A**ssess, **P**arallelize, **O**ptimize, **D**eploy. In short, APOD prescribes an iterative design process, where developers can apply incremental improvements to their accelerated application's performance, and ship their code. As developers become more competent CUDA programmers, more advanced optimization techniques can be applied to their accelerated codebases.

This lab will support such a style of iterative development. You will be using the **NVIDIA Command Line Profiler** to qualitatively measure your application's performance, and to identify opportunities for optimization, after which you will apply incremental improvements before learning new techniques and repeating the cycle. As a point of focus, many of the techniques you will be learning and applying in this lab will deal with the specifics of how CUDA's **Unified Memory** works. Understanding Unified Memory behavior is a fundamental skill for CUDA developers, and serves as a prerequisite to many more advanced memory management techniques.

---
## Prerequisites

To get the most out of this lab you should already be able to:

- Write, compile, and run C/C++ programs that both call CPU functions and launch GPU kernels.
- Control parallel thread hierarchy using execution configuration.
- Refactor serial loops to execute their iterations in parallel on a GPU.
- Allocate and free Unified Memory.

---
## Objectives

By the time you complete this lab, you will be able to:

- Use the **NVIDIA Command Line Profiler** (**nprof**) to profile accelerated application performance.
- Leverage an understanding of **Streaming Multiprocessors** to optimize execution configurations.
- Understand the behavior of **Unified Memory** with regard to page faulting and data migrations.
- Use **asynchronous memory prefetching** to reduce page faults and data migrations for increased performance.
- Employ an iterative development cycle to rapidly accelerate and deploy applications.

---
## Iterative Optimizations with the NVIDIA Command Line Profiler

The only way to be assured that attempts at optimizing accelerated code bases are actually successful is to profile the application for quantitative information about the application's performance. `nvprof` is the NVIDIA command line profiler. It ships with the CUDA toolkit, and is a powerful tool for profiling accelerated applications.

`nvprof` is easy to use. Its most basic usage is to simply pass it the path to an executable compiled with `nvcc`. `nvprof` will proceed to execute the application, after which it will print a summary output of the application's GPU activities, CUDA API calls, as well as information about **Unified Memory** activity, a topic which will be covered extensively later in this lab.

When accelerating applications, or optimizing already-accelerated applications, take a scientific and iterative approach. Profile your application after making changes, take note, and record the implications of any refactoring on performance. Make these observations early and often: frequently, enough performance boost can be gained with little effort such that you can ship your accelerated application. Additionally, frequent profiling will teach you how specific changes to your CUDA codebases impact its actual performance: knowledge that is hard to acquire when only profiling after many kinds of changes in your codebase.


### Exercise: Profile an Application with nvprof

[01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu) (<------ you can click on this and any of the source file links in this lab to open them for editing) is a naively accelerated vector addition program. Use the two code execution cells below (by `CTRL` + clicking them). The first code execution cell will compile (and run) the vector addition program. The second code execution cell will profile the executable that was just compiled using `nvprof`.

After profiling the application, answer the following questions using information displayed in the profiling output:

- What was the name of the only CUDA kernel called in this application?
- How many times did this kernel run?
- How long did it take this kernel to run? Record this time somewhere: you will be optimizing this application and will want to know how much faster you can make it.


```python
!nvcc -arch=sm_70 -o single-thread-vector-add 01-vector-add/01-vector-add.cu -run
```

    Success! All values calculated correctly.



```python
!nvprof ./single-thread-vector-add
```

    ==189== NVPROF is profiling process 189, command: ./single-thread-vector-add
    Success! All values calculated correctly.
    ==189== Profiling application: ./single-thread-vector-add
    ==189== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:  100.00%  2.35807s         1  2.35807s  2.35807s  2.35807s  addVectorsInto(float*, float*, float*, int)
          API calls:   59.59%  2.35807s         1  2.35807s  2.35807s  2.35807s  cudaDeviceSynchronize
                       39.80%  1.57489s         3  524.96ms  20.009us  1.57483s  cudaMallocManaged
                        0.60%  23.602ms         3  7.8673ms  7.1907ms  9.1217ms  cudaFree
                        0.01%  256.45us        94  2.7280us     618ns  70.991us  cuDeviceGetAttribute
                        0.01%  248.77us         1  248.77us  248.77us  248.77us  cuDeviceTotalMem
                        0.00%  132.63us         1  132.63us  132.63us  132.63us  cudaLaunch
                        0.00%  17.458us         1  17.458us  17.458us  17.458us  cuDeviceGetName
                        0.00%  10.438us         4  2.6090us     691ns  7.6800us  cudaSetupArgument
                        0.00%  4.8030us         1  4.8030us  4.8030us  4.8030us  cudaConfigureCall
                        0.00%  4.2080us         3  1.4020us     628ns  2.4430us  cuDeviceGetCount
                        0.00%  1.7500us         2     875ns     675ns  1.0750us  cuDeviceGet
                        0.00%     824ns         1     824ns     824ns     824ns  cudaGetLastError
    
    ==189== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
        2304  170.67KB  4.0000KB  0.9961MB  384.0000MB  41.99990ms  Host To Device
         768  170.67KB  4.0000KB  0.9961MB  128.0000MB  11.28739ms  Device To Host
         768         -         -         -           -  113.3311ms  Gpu page fault groups
    Total CPU Page faults: 1536


### Exercise: Optimize and Profile

Take a minute or two to make a simple optimization to [01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu) by updating its execution configuration so that it runs on many threads in a single thread block. Recompile and then profile with `nvprof` using the code execution cells below. Use the profiling output to check the runtime of the kernel. What was the speed up from this optimization? Be sure to record your results somewhere.


```python
!nvcc -arch=sm_70 -o multi-thread-vector-add 01-vector-add/01-vector-add.cu -run
```

    Success! All values calculated correctly.



```python
!nvprof ./multi-thread-vector-add
```

    ==243== NVPROF is profiling process 243, command: ./multi-thread-vector-add
    Success! All values calculated correctly.
    ==243== Profiling application: ./multi-thread-vector-add
    ==243== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:  100.00%  139.06ms         1  139.06ms  139.06ms  139.06ms  addVectorsInto(float*, float*, float*, int)
          API calls:   49.70%  161.17ms         3  53.723ms  21.781us  161.10ms  cudaMallocManaged
                       42.88%  139.07ms         1  139.07ms  139.07ms  139.07ms  cudaDeviceSynchronize
                        7.21%  23.393ms         3  7.7976ms  7.0885ms  9.1326ms  cudaFree
                        0.08%  257.34us        94  2.7370us     631ns  70.732us  cuDeviceGetAttribute
                        0.08%  248.88us         1  248.88us  248.88us  248.88us  cuDeviceTotalMem
                        0.04%  129.30us         1  129.30us  129.30us  129.30us  cudaLaunch
                        0.01%  17.322us         1  17.322us  17.322us  17.322us  cuDeviceGetName
                        0.00%  10.278us         4  2.5690us     670ns  7.6260us  cudaSetupArgument
                        0.00%  4.3330us         1  4.3330us  4.3330us  4.3330us  cudaConfigureCall
                        0.00%  3.5360us         3  1.1780us     600ns  1.8940us  cuDeviceGetCount
                        0.00%  1.7350us         2     867ns     706ns  1.0290us  cuDeviceGet
                        0.00%     833ns         1     833ns     833ns     833ns  cudaGetLastError
    
    ==243== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
        2304  170.67KB  4.0000KB  0.9961MB  384.0000MB  42.19565ms  Host To Device
         768  170.67KB  4.0000KB  0.9961MB  128.0000MB  11.32054ms  Device To Host
         785         -         -         -           -  110.6570ms  Gpu page fault groups
    Total CPU Page faults: 1536


### Exercise: Optimize Iteratively

In this exercise you will go through several cycles of editing the execution configuration of [01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu), profiling it, and recording the results to see the impact. Use the following guidelines while working:

- Start by listing 3 to 5 different ways you will update the execution configuration, being sure to cover a range of different grid and block size combinations.
- Edit the [01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu) program in one of the ways you listed.
- Compile and profile your updated code with the two code execution cells below.
- Record the runtime of the kernel execution, as given in the profiling output.
- Repeat the edit/profile/record cycle for each possible optimzation you listed above

Which of the execution configurations you attempted proved to be the fastest?


```python
!nvcc -arch=sm_70 -o iteratively-optimized-vector-add 01-vector-add/01-vector-add.cu -run
```

    Success! All values calculated correctly.



```python
!nvprof ./iteratively-optimized-vector-add
```

    ==405== NVPROF is profiling process 405, command: ./iteratively-optimized-vector-add
    Success! All values calculated correctly.
    ==405== Profiling application: ./iteratively-optimized-vector-add
    ==405== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:  100.00%  121.72ms         1  121.72ms  121.72ms  121.72ms  addVectorsInto(float*, float*, float*, int)
          API calls:   52.83%  163.42ms         3  54.475ms  20.635us  163.36ms  cudaMallocManaged
                       39.35%  121.72ms         1  121.72ms  121.72ms  121.72ms  cudaDeviceSynchronize
                        7.60%  23.507ms         3  7.8355ms  7.1717ms  9.0865ms  cudaFree
                        0.08%  257.05us        94  2.7340us     611ns  72.027us  cuDeviceGetAttribute
                        0.08%  248.64us         1  248.64us  248.64us  248.64us  cuDeviceTotalMem
                        0.05%  145.58us         1  145.58us  145.58us  145.58us  cudaLaunch
                        0.01%  17.746us         1  17.746us  17.746us  17.746us  cuDeviceGetName
                        0.00%  10.030us         4  2.5070us     706ns  7.1170us  cudaSetupArgument
                        0.00%  4.8080us         1  4.8080us  4.8080us  4.8080us  cudaConfigureCall
                        0.00%  3.9490us         3  1.3160us     673ns  2.0890us  cuDeviceGetCount
                        0.00%  2.0680us         2  1.0340us     742ns  1.3260us  cuDeviceGet
                        0.00%     888ns         1     888ns     888ns     888ns  cudaGetLastError
    
    ==405== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
        4814  28.312KB  4.0000KB  0.9922MB  133.0977MB  24.82310ms  Host To Device
         766  171.03KB  4.0000KB  0.9961MB  127.9375MB  11.31667ms  Device To Host
         104         -         -         -           -  42.37683ms  Gpu page fault groups
    Total CPU Page faults: 1536


---
## Streaming Multiprocessors and Querying the Device

This section explores how understanding a specific feature of the GPU hardware can promote optimization. After introducing **Streaming Multiprocessors**, you will attempt to further optimize the accelerated vector addition program you have been working on.

The following slides present upcoming material visually, at a high level. Click through the slides before moving on to more detailed coverage of their topics in following sections.


```python
%%HTML

<div align="center"><iframe src="https://docs.google.com/presentation/d/e/2PACX-1vQTzaK1iaFkxgYxaxR5QgHCVx1ZqhpX2F3q9UU6sGKCYaNIq6CGAo8W_qyzg2qwpeiZoHd7NCug7OTj/embed?start=false&loop=false&delayms=3000" frameborder="0" width="900" height="550" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe></div>
```



<div align="center"><iframe src="https://docs.google.com/presentation/d/e/2PACX-1vQTzaK1iaFkxgYxaxR5QgHCVx1ZqhpX2F3q9UU6sGKCYaNIq6CGAo8W_qyzg2qwpeiZoHd7NCug7OTj/embed?start=false&loop=false&delayms=3000" frameborder="0" width="900" height="550" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe></div>


### Streaming Multiprocessors and Warps

The GPUs that CUDA applications run on have processing units called **streaming multiprocessors**, or **SMs**. During kernel execution, blocks of threads are given to SMs to execute. In order to support the GPU's ability to perform as many parallel operations as possible, performance gains can often be had by *choosing a grid size that has a number of blocks that is a multiple of the number of SMs on a given GPU.*

Additionally, SMs create, manage, schedule, and execute groupings of 32 threads from within a block called **warps**. A more [in depth coverage of SMs and warps](http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#hardware-implementation) is beyond the scope of this course, however, it is important to know that performance gains can also be had by *choosing a block size that has a number of threads that is a multiple of 32.*

### Programmatically Querying GPU Device Properties

In order to support portability, since the number of SMs on a GPU can differ depending on the specific GPU being used, the number of SMs should not be hard-coded into a codebase. Rather, this information should be acquired programatically.

The following shows how, in CUDA C/C++, to obtain a C struct which contains many properties about the currently active GPU device, including its number of SMs:

```cpp
int deviceId;
cudaGetDevice(&deviceId);                  // `deviceId` now points to the id of the currently active GPU.

cudaDeviceProp props;
cudaGetDeviceProperties(&props, deviceId); // `props` now has many useful properties about
                                           // the active GPU device.
```

### Exercise: Query the Device

Currently, [`01-get-device-properties.cu`](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/04-device-properties/01-get-device-properties.cu) contains many unassigned variables, and will print gibberish information intended to describe details about the currently active GPU.

Build out [`01-get-device-properties.cu`](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/04-device-properties/01-get-device-properties.cu) to print the actual values for the desired device properties indicated in the source code. In order to support your work, and as an introduction to them, use the [CUDA Runtime Docs](http://docs.nvidia.com/cuda/cuda-runtime-api/structcudaDeviceProp.html) to help identify the relevant properties in the device props struct. Refer to [the solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/04-device-properties/solutions/01-get-device-properties-solution.cu) if you get stuck.


```python
!nvcc -arch=sm_70 -o get-device-properties 04-device-properties/01-get-device-properties.cu -run
```

    Device ID: 0
    Number of SMs: 80
    Compute Capability Major: 7
    Compute Capability Minor: 0
    Warp Size: 32


### Exercise: Optimize Vector Add with Grids Sized to Number of SMs

Utilize your ability to query the device for its number of SMs to refactor the `addVectorsInto` kernel you have been working on inside [01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu) so that it launches with a grid containing a number of blocks that is a multiple of the number of SMs on the device.

Depending on other specific details in the code you have written, this refactor may or may not improve, or significantly change, the performance of your kernel. Therefore, as always, be sure to use `nvprof` so that you can quantitatively evaulate performance changes. Record the results with the rest of your findings thus far, based on the profiling output.


```python
!nvcc -arch=sm_70 -o sm-optimized-vector-add 01-vector-add/01-vector-add.cu -run
```

    Success! All values calculated correctly.



```python
!nvprof ./sm-optimized-vector-add
```

    ==575== NVPROF is profiling process 575, command: ./sm-optimized-vector-add
    Success! All values calculated correctly.
    ==575== Profiling application: ./sm-optimized-vector-add
    ==575== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:  100.00%  109.23ms         1  109.23ms  109.23ms  109.23ms  addVectorsInto(float*, float*, float*, int)
          API calls:   55.18%  164.62ms         3  54.872ms  18.793us  164.56ms  cudaMallocManaged
                       36.60%  109.20ms         1  109.20ms  109.20ms  109.20ms  cudaDeviceSynchronize
                        7.89%  23.549ms         3  7.8497ms  7.1173ms  9.1678ms  cudaFree
                        0.09%  256.21us        94  2.7250us     616ns  71.247us  cuDeviceGetAttribute
                        0.08%  251.19us         1  251.19us  251.19us  251.19us  cudaGetDeviceProperties
                        0.08%  247.96us         1  247.96us  247.96us  247.96us  cuDeviceTotalMem
                        0.06%  181.34us         1  181.34us  181.34us  181.34us  cudaLaunch
                        0.01%  17.269us         1  17.269us  17.269us  17.269us  cuDeviceGetName
                        0.00%  8.7950us         1  8.7950us  8.7950us  8.7950us  cudaGetDevice
                        0.00%  3.8670us         4     966ns     765ns  1.2330us  cudaSetupArgument
                        0.00%  3.7920us         3  1.2640us     593ns  2.0370us  cuDeviceGetCount
                        0.00%  3.1870us         1  3.1870us  3.1870us  3.1870us  cudaConfigureCall
                        0.00%  1.7880us         2     894ns     705ns  1.0830us  cuDeviceGet
                        0.00%     789ns         1     789ns     789ns     789ns  cudaGetLastError
    
    ==575== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
        6990  32.279KB  4.0000KB  0.9922MB  220.3477MB  38.64310ms  Host To Device
         766  171.03KB  4.0000KB  0.9961MB  127.9375MB  11.32765ms  Device To Host
         125         -         -         -           -  62.17808ms  Gpu page fault groups
    Total CPU Page faults: 1536


---
## Unified Memory Details

You have been allocting memory intended for use either by host or device code with `cudaMallocManaged` and up until now have enjoyed the benefits of this method - automatic memory migration, ease of programming - without diving into the details of how the **Unified Memory** (**UM**) allocated by `cudaMallocManaged` actual works. `nvprof` provides details about UM management in accelerated applications, and using this information, in conjunction with a more-detailed understanding of how UM works, provides additional opportunities to optimize accelerated applications.

The following slides present upcoming material visually, at a high level. Click through the slides before moving on to more detailed coverage of their topics in following sections.


```python
%%HTML

<div align="center"><iframe src="https://docs.google.com/presentation/d/e/2PACX-1vS0-BCGiWUb82r1RH-4cSRmZjN2vjebqoodlHIN1fvtt1iDh8X8W9WOSlLVxcsY747WVIebw13cDYBO/embed?start=false&loop=false&delayms=3000" frameborder="0" width="900" height="550" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe></div>
```



<div align="center"><iframe src="https://docs.google.com/presentation/d/e/2PACX-1vS0-BCGiWUb82r1RH-4cSRmZjN2vjebqoodlHIN1fvtt1iDh8X8W9WOSlLVxcsY747WVIebw13cDYBO/embed?start=false&loop=false&delayms=3000" frameborder="0" width="900" height="550" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe></div>


### Unified Memory Migration

When UM is allocated, the memory is not resident yet on either the host or the device. When either the host or device attempts to access the memory, a [page fault](https://en.wikipedia.org/wiki/Page_fault) will occur, at which point the host or device will migrate the needed data in batches. Similarly, at any point when the CPU, or any GPU in the accelerated system, attempts to access memory not yet resident on it, page faults will occur and trigger its migration.

The ability to page fault and migrate memory on demand is tremendously helpful for ease of development in your accelerated applications. Additionally, when working with data that exhibits sparse access patterns, for example when it is impossible to know which data will be required to be worked on until the application actually runs, and for scenarios when data might be accessed by multiple GPU devices in an accelerated system with multiple GPUs, on-demand memory migration is remarkably beneficial.

There are times - for example when data needs are known prior to runtime, and large contiguous blocks of memory are required - when the overhead of page faulting and migrating data on demand incurs an overhead cost that would be better avoided.

Much of the remainder of this lab will be dedicated to understanding on-demand migration, and how to identify it in the profiler's output. With this knowledge you will be able to reduce the overhead of it in scenarios when it would be beneficial.

### Exercise: Explore UM Page Faulting

`nvprof` provides output describing UM behavior for the profiled application. In this exercise, you will make several modifications to a simple application, and make use of `nvprof`'s Unified Memory output section after each change, to explore how UM data migration behaves.

[`01-page-faults.cu`](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/06-unified-memory-page-faults/01-page-faults.cu) contains a `hostFunction` and a `gpuKernel`, both which could be used to initialize the elements of a `2<<24` element vector with the number `1`. Curently neither the host function nor GPU kernel are being used.

For each of the 4 questions below, given what you have just learned about UM behavior, first hypothesize about what kind of page faulting should happen, then, edit [`01-page-faults.cu`](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/06-unified-memory-page-faults/01-page-faults.cu) to create a scenario, by using one or both of the 2 provided functions in the codebase, that will allow you to test your hypothesis.

In order to test your hypotheses, compile and profile your code using the code execution cells below. Be sure to record your hypotheses, as well as the results, obtained from `nvprof` output, specifically CPU and GPU page faults, for each of the 4 experiments you are conducting. There are links to solutions for each of the 4 experiments which you can refer to if you get stuck.

- What happens when unified memory is accessed only by the CPU? ([solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/06-unified-memory-page-faults/solutions/01-page-faults-solution-cpu-only.cu))
- What happens when unified memory is accessed only by the GPU? ([solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/06-unified-memory-page-faults/solutions/02-page-faults-solution-gpu-only.cu))
- What happens when unified memory is accessed first by the CPU then the GPU? ([solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/06-unified-memory-page-faults/solutions/03-page-faults-solution-cpu-then-gpu.cu))
- What happens when unified memory is accessed first by the GPU then the CPU? ([solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/06-unified-memory-page-faults/solutions/04-page-faults-solution-gpu-then-cpu.cu))


```python
!nvcc -arch=sm_70 -o page-faults 06-unified-memory-page-faults/01-page-faults.cu -run
```


```python
!nvprof ./page-faults
```

    ==899== NVPROF is profiling process 899, command: ./page-faults
    ==899== Profiling application: ./page-faults
    ==899== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:  100.00%  32.267ms         1  32.267ms  32.267ms  32.267ms  deviceKernel(int*, int)
          API calls:   80.90%  162.84ms         1  162.84ms  162.84ms  162.84ms  cudaMallocManaged
                       16.03%  32.273ms         1  32.273ms  32.273ms  32.273ms  cudaDeviceSynchronize
                        2.74%  5.5215ms         1  5.5215ms  5.5215ms  5.5215ms  cudaFree
                        0.13%  256.44us        94  2.7280us     606ns  71.861us  cuDeviceGetAttribute
                        0.12%  246.61us         1  246.61us  246.61us  246.61us  cuDeviceTotalMem
                        0.05%  107.79us         1  107.79us  107.79us  107.79us  cudaLaunch
                        0.01%  17.348us         1  17.348us  17.348us  17.348us  cuDeviceGetName
                        0.00%  6.5210us         2  3.2600us     699ns  5.8220us  cudaSetupArgument
                        0.00%  3.8570us         3  1.2850us     606ns  2.3060us  cuDeviceGetCount
                        0.00%  2.1350us         1  2.1350us  2.1350us  2.1350us  cudaConfigureCall
                        0.00%  1.7240us         2     862ns     677ns  1.0470us  cuDeviceGet
    
    ==899== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
         381         -         -         -           -  32.14630ms  Gpu page fault groups


### Exercise: Revisit UM Behavior for Vector Add Program

Returning to the [01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu) program you have been working on throughout this lab, review the codebase in its current state, and hypothesize about what kinds of page faults you expect to occur. Look at the profiling output for your last refactor (either by scrolling up to find the output or by executing the code execution cell just below), observing the Unified Memory section of the profiler output. Can you explain the page faulting descriptions based on the contents of the code base?


```python
!nvprof ./sm-optimized-vector-add
```

    ==911== NVPROF is profiling process 911, command: ./sm-optimized-vector-add
    Success! All values calculated correctly.
    ==911== Profiling application: ./sm-optimized-vector-add
    ==911== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:  100.00%  134.31ms         1  134.31ms  134.31ms  134.31ms  addVectorsInto(float*, float*, float*, int)
          API calls:   50.47%  162.73ms         3  54.244ms  23.906us  162.67ms  cudaMallocManaged
                       41.65%  134.28ms         1  134.28ms  134.28ms  134.28ms  cudaDeviceSynchronize
                        7.57%  24.422ms         3  8.1406ms  7.1919ms  9.1551ms  cudaFree
                        0.08%  255.06us        94  2.7130us     609ns  71.291us  cuDeviceGetAttribute
                        0.08%  248.76us         1  248.76us  248.76us  248.76us  cuDeviceTotalMem
                        0.08%  248.50us         1  248.50us  248.50us  248.50us  cudaGetDeviceProperties
                        0.06%  189.23us         1  189.23us  189.23us  189.23us  cudaLaunch
                        0.01%  17.807us         1  17.807us  17.807us  17.807us  cuDeviceGetName
                        0.00%  8.8770us         1  8.8770us  8.8770us  8.8770us  cudaGetDevice
                        0.00%  4.2550us         3  1.4180us     624ns  2.3390us  cuDeviceGetCount
                        0.00%  4.0390us         4  1.0090us     757ns  1.3450us  cudaSetupArgument
                        0.00%  3.0740us         1  3.0740us  3.0740us  3.0740us  cudaConfigureCall
                        0.00%  1.8790us         2     939ns     711ns  1.1680us  cuDeviceGet
                        0.00%  1.1780us         1  1.1780us  1.1780us  1.1780us  cudaGetLastError
    
    ==911== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
        6958  33.762KB  4.0000KB  0.9961MB  229.4141MB  39.33427ms  Host To Device
         766  171.03KB  4.0000KB  0.9961MB  127.9375MB  11.31690ms  Device To Host
         135         -         -         -           -  82.91155ms  Gpu page fault groups
    Total CPU Page faults: 1536


### Exercise: Initialize Vector in Kernel

When `nvprof` gives the amount of time that a kernel takes to execute, the host-to-device page faults and data migrations that occur during this kernel's execution are included in the displayed execution time.

With this in mind, refactor the `initWith` host function in your [01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu) program to instead be a CUDA kernel, initializing the allocated vector in parallel on the GPU. After successfully compiling and running the refactored application, but before profiling it, hypothesize about the following:

- How do you expect the refactor to affect UM page-fault behavior?
- How do you expect the refactor to affect the reported run time of `addVectorsInto`?

Once again, record the results. Refer to [the solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/07-init-in-kernel/solutions/01-vector-add-init-in-kernel-solution.cu) if you get stuck.


```python
!nvcc -arch=sm_70 -o initialize-in-kernel 01-vector-add/01-vector-add.cu -run
```

    Success! All values calculated correctly.



```python
!nvprof ./initialize-in-kernel
```

    ==965== NVPROF is profiling process 965, command: ./initialize-in-kernel
    Success! All values calculated correctly.
    ==965== Profiling application: ./initialize-in-kernel
    ==965== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:   99.21%  61.119ms         3  20.373ms  19.865ms  21.340ms  initWith(float, float*, int)
                        0.79%  489.20us         1  489.20us  489.20us  489.20us  addVectorsInto(float*, float*, float*, int)
          API calls:   66.43%  163.38ms         3  54.460ms  45.148us  163.25ms  cudaMallocManaged
                       25.03%  61.567ms         1  61.567ms  61.567ms  61.567ms  cudaDeviceSynchronize
                        8.13%  20.006ms         3  6.6687ms  5.5569ms  8.7020ms  cudaFree
                        0.11%  269.65us        94  2.8680us     607ns  71.521us  cuDeviceGetAttribute
                        0.11%  265.32us         1  265.32us  265.32us  265.32us  cudaGetDeviceProperties
                        0.10%  247.43us         1  247.43us  247.43us  247.43us  cuDeviceTotalMem
                        0.06%  150.78us         4  37.694us  8.1610us  121.53us  cudaLaunch
                        0.01%  17.260us         1  17.260us  17.260us  17.260us  cuDeviceGetName
                        0.00%  9.2880us        13     714ns     590ns  1.0740us  cudaSetupArgument
                        0.00%  4.3160us         4  1.0790us     698ns  1.8790us  cudaConfigureCall
                        0.00%  4.0250us         3  1.3410us     615ns  2.2620us  cuDeviceGetCount
                        0.00%  3.4640us         1  3.4640us  3.4640us  3.4640us  cudaGetDevice
                        0.00%  1.9310us         2     965ns     700ns  1.2310us  cuDeviceGet
                        0.00%     778ns         1     778ns     778ns     778ns  cudaGetLastError
    
    ==965== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
         768  170.67KB  4.0000KB  0.9961MB  128.0000MB  11.38803ms  Device To Host
         328         -         -         -           -  60.92627ms  Gpu page fault groups
    Total CPU Page faults: 384


---
## Asynchronous Memory Prefetching

A powerful technique to reduce the overhead of page faulting and on-demand memory migrations, both in host-to-device and device-to-host memory transfers, is called **asynchronous memory prefetching**. Using this technique allows programmers to asynchronously migrate unified memory (UM) to any CPU or GPU device in the system, in the background, prior to its use by application code. By doing this, GPU kernels and CPU function performance can be increased on account of reduced page fault and on-demand data migration overhead.

Prefetching also tends to migrate data in larger chunks, and therefore fewer trips, than on-demand migration. This makes it an excellent fit when data access needs are known before runtime, and when data access patterns are not sparse.

CUDA Makes asynchronously prefetching managed memory to either a GPU device or the CPU easy with its `cudaMemPrefetchAsync` function. Here is an example of using it to both prefetch data to the currently active GPU device, and then, to the CPU:

```cpp
int deviceId;
cudaGetDevice(&deviceId);                                         // The ID of the currently active GPU device.

cudaMemPrefetchAsync(pointerToSomeUMData, size, deviceId);        // Prefetch to GPU device.
cudaMemPrefetchAsync(pointerToSomeUMData, size, cudaCpuDeviceId); // Prefetch to host. `cudaCpuDeviceId` is a
                                                                  // built-in CUDA variable.
```

### Exercise: Prefetch Memory

At this point in the lab, your [01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu) program should not only be launching a CUDA kernel to add 2 vectors into a third solution vector, all which are allocated with `cudaMallocManaged`, but should also initializing each of the 3 vectors in parallel in a CUDA kernel. If for some reason, your application does not do any of the above, please refer to the following [reference application](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/08-prefetch/01-vector-add-prefetch.cu), and update your own codebase to reflect its current functionality.

Conduct 3 experiments using `cudaMemPrefetchAsync` inside of your [01-vector-add.cu](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/01-vector-add/01-vector-add.cu) application to understand its impact on page-faulting and memory migration.

- What happens when you prefetch one of the initialized vectors to the device?
- What happens when you prefetch two of the initialized vectors to the device?
- What happens when you prefetch all three of the initialized vectors to the device?

Hypothesize about UM behavior, page faulting specificially, as well as the impact on the reported run time of the initialization kernel, before each experiement, and then verify by running `nvprof`. Refer to [the solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/08-prefetch/solutions/01-vector-add-prefetch-solution.cu) if you get stuck.


```python
!nvcc -arch=sm_70 -o prefetch-to-gpu 01-vector-add/01-vector-add.cu -run
```

    Success! All values calculated correctly.



```python
!nvprof ./prefetch-to-gpu
```

    ==1253== NVPROF is profiling process 1253, command: ./prefetch-to-gpu
    Success! All values calculated correctly.
    ==1253== Profiling application: ./prefetch-to-gpu
    ==1253== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:   51.93%  501.75us         1  501.75us  501.75us  501.75us  addVectorsInto(float*, float*, float*, int)
                       48.07%  464.37us         3  154.79us  152.45us  158.65us  initWith(float, float*, int)
          API calls:   82.27%  163.41ms         3  54.470ms  20.840us  163.35ms  cudaMallocManaged
                        9.73%  19.323ms         3  6.4410ms  5.2158ms  8.8392ms  cudaFree
                        4.80%  9.5274ms         1  9.5274ms  9.5274ms  9.5274ms  cudaDeviceSynchronize
                        2.74%  5.4447ms         3  1.8149ms  17.959us  4.8597ms  cudaMemPrefetchAsync
                        0.13%  258.39us        94  2.7480us     639ns  71.641us  cuDeviceGetAttribute
                        0.12%  247.89us         1  247.89us  247.89us  247.89us  cuDeviceTotalMem
                        0.11%  224.79us         1  224.79us  224.79us  224.79us  cudaGetDeviceProperties
                        0.07%  143.27us         4  35.817us  8.0660us  113.28us  cudaLaunch
                        0.01%  18.256us         1  18.256us  18.256us  18.256us  cuDeviceGetName
                        0.01%  10.726us        13     825ns     609ns  1.6580us  cudaSetupArgument
                        0.00%  4.4870us         3  1.4950us     680ns  2.5140us  cuDeviceGetCount
                        0.00%  4.0290us         4  1.0070us     677ns  1.5730us  cudaConfigureCall
                        0.00%  1.8550us         2     927ns     713ns  1.1420us  cuDeviceGet
                        0.00%  1.7650us         1  1.7650us  1.7650us  1.7650us  cudaGetDevice
                        0.00%     767ns         1     767ns     767ns     767ns  cudaGetLastError
    
    ==1253== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
         768  170.67KB  4.0000KB  0.9961MB  128.0000MB  11.39206ms  Device To Host
    Total CPU Page faults: 384


### Exercise: Prefetch Memory Back to the CPU

Add additional prefetching back to the CPU for the function that verifies the correctness of the `addVectorInto` kernel. Again, hypothesize about the impact on UM before profiling in `nvprof` to confirm. Refer to [the solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/08-prefetch/solutions/02-vector-add-prefetch-solution-cpu-also.cu) if you get stuck.


```python
!nvcc -arch=sm_70 -o prefetch-to-cpu 01-vector-add/01-vector-add.cu -run
```

    Success! All values calculated correctly.



```python
!nvprof ./prefetch-to-cpu
```

    ==1589== NVPROF is profiling process 1589, command: ./prefetch-to-cpu
    Success! All values calculated correctly.
    ==1589== Profiling application: ./prefetch-to-cpu
    ==1589== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:   51.86%  499.93us         1  499.93us  499.93us  499.93us  addVectorsInto(float*, float*, float*, int)
                       48.14%  464.15us         3  154.72us  152.80us  157.72us  initWith(float, float*, int)
          API calls:   75.43%  161.71ms         3  53.902ms  20.047us  161.64ms  cudaMallocManaged
                       10.97%  23.509ms         4  5.8773ms  20.994us  18.505ms  cudaMemPrefetchAsync
                        8.75%  18.761ms         3  6.2538ms  5.2629ms  8.1946ms  cudaFree
                        4.42%  9.4678ms         1  9.4678ms  9.4678ms  9.4678ms  cudaDeviceSynchronize
                        0.13%  277.81us        94  2.9550us     611ns  92.918us  cuDeviceGetAttribute
                        0.12%  247.65us         1  247.65us  247.65us  247.65us  cuDeviceTotalMem
                        0.11%  226.63us         1  226.63us  226.63us  226.63us  cudaGetDeviceProperties
                        0.07%  143.42us         4  35.854us  7.8730us  114.51us  cudaLaunch
                        0.01%  17.076us         1  17.076us  17.076us  17.076us  cuDeviceGetName
                        0.00%  9.6720us        13     744ns     587ns  1.2320us  cudaSetupArgument
                        0.00%  4.1420us         4  1.0350us     627ns  1.6610us  cudaConfigureCall
                        0.00%  3.7390us         3  1.2460us     618ns  2.0660us  cuDeviceGetCount
                        0.00%  1.7500us         2     875ns     715ns  1.0350us  cuDeviceGet
                        0.00%  1.6900us         1  1.6900us  1.6900us  1.6900us  cudaGetDevice
                        0.00%     893ns         1     893ns     893ns     893ns  cudaGetLastError
    
    ==1589== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
          64  2.0000MB  2.0000MB  2.0000MB  128.0000MB  10.65178ms  Device To Host


---
## Summary

At this point in the lab, you are able to:

- Use the **NVIDIA Command Line Profiler** (**nvprof**) to profile accelerated application performance.
- Leverage an understanding of **Streaming Multiprocessors** to optimize execution configurations.
- Understand the behavior of **Unified Memory** with regard to page faulting and data migrations.
- Use **asynchronous memory prefetching** to reduce page faults and data migrations for increased performance.
- Employ an iterative development cycle to rapidly accelerate and deploy applications.

In order to consolidate your learning, and reinforce your ability to iteratively accelerate, optimize, and deploy applications, please proceed to this lab's final exercise. After completing it, for those of you with time and interest, please proceed to the *Advanced Content* section.

---
## Final Exercise: Iteratively Optimize an Accelerated SAXPY Application

A basic accelerated [SAXPY](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms#Level_1) application has been provided for you [here](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/09-saxpy/01-saxpy.cu). It currently contains a couple of bugs that you will need to find and fix before you can successfully compile, run, and then profile it with `nvprof`.

After fixing the bugs and profiling the application, record the runtime of the `saxpy` kernel and then work *iteratively* to optimize the application, using `nvprof` after each iteration to notice the effects of the code changes on kernel performance and UM behavior.

Utilize the techniques from this lab. To support your learning, utilize [effortful retrieval](http://sites.gsu.edu/scholarlyteaching/effortful-retrieval/) whenever possible, rather than rushing to look up the specifics of techniques from earlier in the lesson.

Your end goal is to profile an accurate `saxpy` kernel, without modifying `N`, to run in under *50us*. Check out [the solution](../../../../../edit/tasks/task1/task/02_AC_UM_NVPROF/09-saxpy/solutions/02-saxpy-solution.cu) if you get stuck, and feel free to compile and profile it if you wish.


```python
!nvcc -arch=sm_70 -o saxpy 09-saxpy/01-saxpy.cu -run
```

    c[0] = 5, c[1] = 5, c[2] = 5, c[3] = 5, c[4] = 5, 
    c[4194299] = 5, c[4194300] = 5, c[4194301] = 5, c[4194302] = 5, c[4194303] = 5, 



```python
!nvprof ./saxpy
```

    ==2570== NVPROF is profiling process 2570, command: ./saxpy
    c[0] = 5, c[1] = 5, c[2] = 5, c[3] = 5, c[4] = 5, 
    c[4194299] = 5, c[4194300] = 5, c[4194301] = 5, c[4194302] = 5, c[4194303] = 5, 
    ==2570== Profiling application: ./saxpy
    ==2570== Profiling result:
                Type  Time(%)      Time     Calls       Avg       Min       Max  Name
     GPU activities:  100.00%  70.687us         1  70.687us  70.687us  70.687us  saxpy(int*, int*, int*)
          API calls:   93.26%  162.20ms         3  54.066ms  27.284us  162.14ms  cudaMallocManaged
                        3.31%  5.7600ms         1  5.7600ms  5.7600ms  5.7600ms  cudaDeviceSynchronize
                        1.72%  2.9889ms         3  996.29us  893.39us  1.1605ms  cudaFree
                        1.31%  2.2802ms         4  570.06us  14.124us  2.1060ms  cudaMemPrefetchAsync
                        0.15%  255.99us        94  2.7230us     624ns  71.088us  cuDeviceGetAttribute
                        0.14%  246.63us         1  246.63us  246.63us  246.63us  cuDeviceTotalMem
                        0.08%  140.84us         1  140.84us  140.84us  140.84us  cudaLaunch
                        0.01%  17.429us         1  17.429us  17.429us  17.429us  cuDeviceGetName
                        0.01%  10.006us         3  3.3350us     757ns  8.1920us  cudaSetupArgument
                        0.01%  9.2250us         1  9.2250us  9.2250us  9.2250us  cudaGetDevice
                        0.00%  3.8740us         3  1.2910us     627ns  2.1110us  cuDeviceGetCount
                        0.00%  2.3830us         1  2.3830us  2.3830us  2.3830us  cudaConfigureCall
                        0.00%  1.9640us         2     982ns     725ns  1.2390us  cuDeviceGet
                        0.00%  1.3520us         1  1.3520us  1.3520us  1.3520us  cudaDeviceGetAttribute
    
    ==2570== Unified Memory profiling result:
    Device "Tesla V100-SXM2-16GB (0)"
       Count  Avg Size  Min Size  Max Size  Total Size  Total Time  Name
          24  2.0000MB  2.0000MB  2.0000MB  48.00000MB  4.706816ms  Host To Device
           8  2.0000MB  2.0000MB  2.0000MB  16.00000MB  1.332832ms  Device To Host
    Total CPU Page faults: 144

