#!/bin/sh
set -e

REPO="EvanL1/jqr"
INSTALL_DIR="${INSTALL_DIR:-/usr/local/bin}"

# Detect OS and architecture
detect_platform() {
    os=$(uname -s | tr '[:upper:]' '[:lower:]')
    arch=$(uname -m)

    case "$os" in
        linux) os="unknown-linux-gnu" ;;
        darwin) os="apple-darwin" ;;
        *) echo "Unsupported OS: $os"; exit 1 ;;
    esac

    case "$arch" in
        x86_64) arch="x86_64" ;;
        aarch64|arm64) arch="aarch64" ;;
        *) echo "Unsupported architecture: $arch"; exit 1 ;;
    esac

    echo "${arch}-${os}"
}

# Get latest release version
get_latest_version() {
    curl -sL "https://api.github.com/repos/${REPO}/releases/latest" | \
        grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/'
}

main() {
    platform=$(detect_platform)
    version=$(get_latest_version)

    if [ -z "$version" ]; then
        echo "Failed to get latest version"
        exit 1
    fi

    echo "Installing jqr ${version} for ${platform}..."

    url="https://github.com/${REPO}/releases/download/${version}/jqr-${platform}.tar.gz"

    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    curl -sL "$url" | tar xz -C "$tmpdir"

    if [ -w "$INSTALL_DIR" ]; then
        mv "$tmpdir/jqr" "$INSTALL_DIR/jqr"
    else
        sudo mv "$tmpdir/jqr" "$INSTALL_DIR/jqr"
    fi

    chmod +x "$INSTALL_DIR/jqr"

    echo "jqr installed to $INSTALL_DIR/jqr"
}

main
