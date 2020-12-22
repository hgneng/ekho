# Ekho
#
# Linux:
# docker run --rm -it \
#     --device /dev/snd \
#     dosentmatter/ekho -s -30 -v Toisanese 排骨
#
# macOS:
# $ brew install pulseaudio
# $ # Using BSD `sed`
# $ sed -i '' \
#       -e '/#load-module module-native-protocol-tcp/ s/^#//' \
#       /usr/local/Cellar/pulseaudio/*/etc/pulse/default.pa
# $ brew services start pulseaudio
# $ docker run --rm -it \
#       -e PULSE_SERVER=host.docker.internal \
#       --mount type=bind,source=$HOME/.config/pulse,target=/home/pulseaudio/.config/pulse \
#       dosentmatter/ekho -s -30 -v Toisanese 排骨
#
# Windows PowerShell:
# PS> choco install pulseaudio
# PS> $pulseAudioConfigPath = Join-Path `
#         -Path (Get-Command pulseaudio | Split-Path | Split-Path) `
#         -ChildPath 'lib\pulseaudio\tools\etc\pulse\default.pa'
# PS> (Get-Content $pulseAudioConfigPath) `
#         -replace '#(load-module module-native-protocol-tcp)', '$1' |
#     Set-Content $pulseAudioConfigPath
# PS> pulseaudio # Run once to setup files
# PS> pulseaudio --use-pid-file=false --exit-idle-time=-1
# PS> docker run --rm -it `
#         -e PULSE_SERVER=host.docker.internal `
#         --mount type=bind,source=$HOME\.pulse-cookie,target=/home/pulseaudio/.config/pulse/cookie `
#         dosentmatter/ekho -s -30 -v Toisanese 排骨
#
# Windows WSL:
# $ choco.exe install pulseaudio
# $ # Using GNU `sed`
# $ sed -i \
#       -e '/#load-module module-native-protocol-tcp/ s/^#//' \
#       "$(
#           wslpath "$(where.exe pulseaudio.exe | tr -d '\r')" |
#           xargs dirname |
#           xargs dirname
#       )/lib/pulseaudio/tools/etc/pulse/default.pa"
# $ pulseaudio.exe # Run once to setup files
# $ pulseaudio.exe --use-pid-file=false --exit-idle-time=-1
# $ ln -s \
#       "$(wslpath "$(powershell.exe -Command 'Write-Output $HOME\.pulse-cookie')" | tr -d '\r')" \
#       ~/.pulse-cookie
# $ docker run --rm -it \
#       -e PULSE_SERVER=host.docker.internal \
#       --mount type=bind,source=$HOME/.pulse-cookie,target=/home/pulseaudio/.config/pulse/cookie \
#       dosentmatter/ekho -s -30 -v Toisanese 排骨
FROM jess/pulseaudio AS base

USER root

RUN apt-get update && apt-get install --no-install-recommends -y \
    libc6 \
    libespeak-ng1 \
    libpulse0 \
    libsndfile1 \
    && rm -rf /var/lib/apt/lists/*

USER pulseaudio


FROM base AS builder

USER root

RUN apt-get update && apt-get install --no-install-recommends -y \
    ca-certificates \
    curl \
    g++ \
    gcc \
    libc6-dev \
    libespeak-ng-dev \
    libpulse-dev \
    libsndfile1-dev \
    make \
    xz-utils \
    && rm -rf /var/lib/apt/lists/*

RUN curl -L http://sourceforge.net/projects/e-guidedog/files/Ekho/8.5/ekho-8.5.tar.xz -o ekho.tar.xz \
    && mkdir ekho \
    && tar -xJvf ekho.tar.xz -C ekho --strip-components 1

WORKDIR $HOME/ekho

RUN ./configure --disable-dependency-tracking
RUN make
RUN make install

USER pulseaudio


FROM base as runner

USER root

COPY --from=builder /usr/local/bin/ /usr/local/bin/
COPY --from=builder /usr/local/include/ /usr/local/include/
COPY --from=builder /usr/local/share/ekho-data/ /usr/local/share/ekho-data/

USER pulseaudio

ENTRYPOINT [ "ekho" ]
CMD [ "--help" ]
