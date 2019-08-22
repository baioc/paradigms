#include <stdio.h>

int main(void)
{
	int deviceId;
	cudaGetDevice(&deviceId);

	cudaDeviceProp props;
	cudaGetDeviceProperties(&props, deviceId);

	int computeCapabilityMajor = props.major;
	int computeCapabilityMinor = props.minor;
	int multiProcessorCount = props.multiProcessorCount;
	int warpSize = props.warpSize;
	/// << other properties >

	printf(
		"Device ID: %d\nNumber of SMs: %d\nCompute Capability Major: %d\nCompute Capability Minor: %d\nWarp Size: %d\n",
		deviceId, multiProcessorCount, computeCapabilityMajor, computeCapabilityMinor, warpSize
	);
}
