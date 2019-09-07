import setuptools

with open('README.md', 'r') as fh:
    long_description = fh.read()

setuptools.setup(
    name="ufsc-graphs",
    version="0.1.0",
    author="Gabriel B. Sant'Anna",
    author_email="baiocchi.gabriel@gmail.com",
    description="A package for graph discrete data structures and algorithms",
    long_description=long_description,
    long_description_content_type='text/markdown',
    url="https://gitlab.com/baioc/paradigms",
    keywords='UFSC graphs algorithms',
    packages=['ufsc_graphs'],  # setuptools.find_packages()
    package_dir={'ufsc_graphs': 'ufsc_graphs/'},
    # py_modules=['ufsc_graphs.graphs'],
    package_data={'ufsc_graphs': ['_graphs.so']},
    classifiers=[
        'Programming Language :: Python :: 3',
        'Programming Language :: C++',
        'License :: OSI Approved :: Apache Software License',
        'Operating System :: OS Independent',
        'Topic :: Scientific/Engineering :: Mathematics',
        'Intended Audience :: Science/Research',
    ],
    python_requires='>=3.6',
)
