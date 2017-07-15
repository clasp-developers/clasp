FROM cando-compile
MAINTAINER Christian Schafmeister <meister@temple.edu>

# Install nglview for python 2
USER root
RUN python -m pip install pip --upgrade
RUN python -m pip install numpy
RUN python -m pip install ipywidgets==5.2.2
RUN python -m pip install pytraj==1.0.9

RUN git clone https://github.com/arose/nglview /home/app/nglview
RUN cd /home/app/nglview && python setup.py install
# probably need to activate widgetsnbextension
RUN python -m ipykernel install --sys-prefix
RUN python -m jupyter nbextension enable --py --sys-prefix widgetsnbextension
RUN python -m jupyter nbextension install --py --sys-prefix nglview
RUN python -m jupyter nbextension enable --py --sys-prefix nglview
# tested with ipywidgets 5.2.2, notebook 4.2.1

ADD tools/dockerfiles/jupyter/kernel.json /home/app/.local/share/jupyter/kernels/lisp/kernel.json
ADD tools/dockerfiles/jupyter/jupyter_notebook_config.py /home/app/.jupyter/jupyter_notebook_config.py
RUN chown -R app:app /home/app/.jupyter /home/app/.local $HOME/clasp/Contents/Resources/source-code/extensions/cando/src/lisp
USER app



USER root
RUN chown -R app:app /home/app/nglview
USER app

ENTRYPOINT [ "jupyter", "notebook" ]
CMD        [ "--no-browser", "--ip=0.0.0.0", "--port=8888" ]
