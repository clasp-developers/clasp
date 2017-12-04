FROM cando-compile
MAINTAINER Christian Schafmeister <meister@temple.edu>

# Overwrite the clasp-build clasp/Contents/Resources/source-code/extensions/cando/src/lisp directory
# with whatever new changes have been made in the hosts  extensions/cando/src/lisp/**   hierarchy
#    This is to refresh the Common Lisp code for Cando
ADD extensions/cando/src/lisp $HOME/clasp/Contents/Resources/source-code/extensions/cando/src/lisp

# Install nglview for python 2
USER root
RUN python -m pip install pip --upgrade
RUN python -m pip install numpy
# Added Dec 4, 2017
RUN python -m pip install jupyter
RUN python -m pip install pytraj==1.0.9

### Previous version of nglview only worked with 5.2.2
#RUN python -m pip install ipywidgets==5.2.2
#RUN git clone https://github.com/arose/nglview /home/app/nglview

### New version of nglview works with 7.0
RUN python -m pip install ipywidgets==7.0.0b7
RUN git clone -b "v1.0.a1" https://github.com/arose/nglview /home/app/nglview

RUN cd /home/app/nglview && python setup.py install
# probably need to activate widgetsnbextension
RUN python -m ipykernel install --sys-prefix
RUN python -m jupyter nbextension enable --py --sys-prefix widgetsnbextension
RUN python -m jupyter nbextension install --py --sys-prefix nglview
RUN python -m jupyter nbextension enable --py --sys-prefix nglview

# Install sudo
RUN apt-get install -y sudo

ADD tools/dockerfiles/jupyter/kernel.json /home/app/.local/share/jupyter/kernels/lisp/kernel.json
ADD tools/dockerfiles/jupyter/jupyter_notebook_config.py /home/app/.jupyter/jupyter_notebook_config.py
RUN chown -R app:app /home/app/.jupyter /home/app/.local $HOME/clasp/Contents/Resources/source-code/extensions/cando/src/lisp
ADD tools/dockerfiles/start-demos.sh /home/app/start-demos.sh

RUN mkdir /home/app/work
RUN chown -R app:app /home/app/work
RUN chown -R app:app /home/app/.jupyter
RUN chown -R app:app /home/app/nglview
USER app

ENV HOME /home/app/
ENTRYPOINT [ "jupyter", "notebook" ]
CMD        [ "--no-browser", "--ip=0.0.0.0", "--port=8888" ]
