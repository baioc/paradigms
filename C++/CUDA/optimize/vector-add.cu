#include <stdio.h>

__global__
void initWith(float num, float *a, int N)
{
	int index = threadIdx.x + blockIdx.x * blockDim.x;
	int stride = blockDim.x * gridDim.x;

	for (int i = index; i < N; i += stride)
		a[i] = num;
}

__global__
void addVectorsInto(float *result, float *a, float *b, int N)
{
	int index = threadIdx.x + blockIdx.x * blockDim.x;
	int stride = blockDim.x * gridDim.x;

	for (int i = index; i < N; i += stride)
		result[i] = a[i] + b[i];
}

void checkElementsAre(float target, float *vector, int N)
{
	for (int i = 0; i < N; i++) {
		if (vector[i] != target) {
			printf("FAIL: vector[%d] - %0.0f does not equal %0.0f\n", i, vector[i], target);
			exit(1);
		}
	}
	printf("Success! All values calculated correctly.\n");
}

int main()
{
	int deviceId;
	int numberOfSMs;

	cudaGetDevice(&deviceId);
	cudaDeviceGetAttribute(&numberOfSMs, cudaDevAttrMultiProcessorCount, deviceId);

	const int N = 2 << 24;
	size_t size = N * sizeof(float);

	float *a;
	float *b;
	float *c;
	float *h_c;

	cudaMalloc(&a, size);
	cudaMalloc(&b, size);
	cudaMalloc(&c, size);
	cudaMallocHost(&h_c, size);

	size_t threadsPerBlock;
	size_t numberOfBlocks;

	threadsPerBlock = 256;
	numberOfBlocks = 32 * numberOfSMs;

	/*
	 * Create 3 streams to run initialize the 3 data vectors in parallel.
	 */
	cudaStream_t stream1, stream2, stream3;
	cudaStreamCreate(&stream1);
	cudaStreamCreate(&stream2);
	cudaStreamCreate(&stream3);

	const int numberOfSegments = 4;                  // This example demonstrates slicing the work into 4 segments.
	int segmentN = N / numberOfSegments;             // A value for a segment's worth of `N` is needed.
	size_t segmentSize = size / numberOfSegments;    // A value for a segment's worth of `size` is needed.

	// For each of the 4 segments...
	for (int i = 0; i < numberOfSegments; ++i) {
		// Calculate the index where this particular segment should operate within the larger arrays.
		size_t segmentOffset = i * segmentN;

		// Create a stream for this segment's worth of copy and work.
		cudaStream_t stream;
		cudaStreamCreate(&stream);

		/*
		 * Give each `initWith` launch its own non-standard stream.
		 */
		initWith<<<numberOfBlocks, threadsPerBlock, 0, stream1>>>(3, a, segmentN);
		initWith<<<numberOfBlocks, threadsPerBlock, 0, stream2>>>(4, b, segmentN);
		initWith<<<numberOfBlocks, threadsPerBlock, 0, stream3>>>(0, c, segmentN);

		// Execute segment's worth of work over same non-default stream as memory copy.
		addVectorsInto<<<numberOfBlocks, threadsPerBlock, 0, stream>>>(&c[segmentOffset], &a[segmentOffset], &b[segmentOffset], segmentN);

		// Asynchronously copy segment's worth of device memory to pinned host mem to over non-default stream.
		cudaMemcpyAsync(&h_c[segmentOffset],    // Take care to access correct location in array.
		                &c[segmentOffset],      // Take care to access correct location in array.
		                segmentSize,            // Only copy a segment's worth of memory.
		                cudaMemcpyDeviceToHost, // Copy direction.
		                stream);                // Provide optional argument for non-default stream.

		// `cudaStreamDestroy` will return immediately (is non-blocking), but will not actually destroy stream until
		// all stream operations are complete, so this is safe.
		cudaStreamDestroy(stream);
	}

	cudaDeviceSynchronize();
	checkElementsAre(7, h_c, N);

	cudaStreamDestroy(stream1);
	cudaStreamDestroy(stream2);
	cudaStreamDestroy(stream3);

	cudaFree(a);
	cudaFree(b);
	cudaFree(c);
	cudaFreeHost(h_c);
}
