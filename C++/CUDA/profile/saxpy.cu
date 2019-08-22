#include <stdio.h>

#define N (2048 * 2048) // Number of elements in each vector
#define X (2)

__global__
void saxpy(int *a, int *b, int *c)
{
	int index = threadIdx.x + blockIdx.x*blockDim.x;
	int stride = blockDim.x * gridDim.x;

	for (int i = index; i < N; i += stride)
		c[i] = a[i]*X + b[i];
}

int main(void)
{
	int *a, *b, *c;
	size_t size = N * sizeof(int);

	int deviceId;
	cudaGetDevice(&deviceId);

	int numberOfSMs;
	cudaDeviceGetAttribute(&numberOfSMs, cudaDevAttrMultiProcessorCount, deviceId);

	cudaMallocManaged(&a, size);
	cudaMallocManaged(&b, size);
	cudaMallocManaged(&c, size);

	for (int i = 0; i < N; ++i) {
		a[i] = 2;
		b[i] = 1;
		c[i] = 0;
	}

	cudaMemPrefetchAsync(a, size, deviceId);
	cudaMemPrefetchAsync(b, size, deviceId);
	cudaMemPrefetchAsync(c, size, deviceId);

	size_t threads_per_block = 256;
	size_t number_of_blocks = numberOfSMs * 32;

	saxpy<<<number_of_blocks,threads_per_block>>>(a, b, c);

	cudaDeviceSynchronize();

	// Print out the first and last 5 values of c for a quality check
	for (int i = 0; i < 5; ++i)
			printf("c[%d] = %d, ", i, c[i]);
	printf ("\n");
	for (int i = N-5; i < N; ++i)
			printf("c[%d] = %d, ", i, c[i]);
	printf ("\n");

	cudaFree(a);
	cudaFree(b);
	cudaFree(c);
}
