import setuptools

with open('README.md', 'r') as fh:
    long_description = fh.read()

setuptools.setup(
    name="ufsc-graphs",
    version="0.1.1",
    author="Gabriel B. Sant'Anna",
    author_email="baiocchi.gabriel@gmail.com",
    description="A package for the study of graph discrete data structures and algorithms",
    long_description=long_description,
    long_description_content_type='text/markdown',
    url="https://gitlab.com/baioc/paradigms/tree/master/Python/graphs",
    keywords='UFSC graphs algorithms',
    packages=['ufsc_graphs'],  # setuptools.find_packages()
    package_dir={'ufsc_graphs': 'ufsc_graphs/'},
    # py_modules=['ufsc_graphs.graphs'],
    package_data={'ufsc_graphs': ['_graphs.so']},
    classifiers=[
        'Operating System :: OS Independent',
        'License :: OSI Approved :: Apache Software License',
        'Topic :: Scientific/Engineering :: Mathematics',
        'Intended Audience :: Science/Research',
        'Programming Language :: Python :: 3',
        'Programming Language :: C++',
    ],
    python_requires='>=3.6',
)
