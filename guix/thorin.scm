(use-modules (gnu))

(use-modules (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules cups
                     desktop
                     networking
                     ssh
                     xorg)

(use-package-modules emacs
                     fonts
                     terminals
                     version-control
                     wm)

(define %thorin-packages
  (append
   (list
    (specification->package "emacs")
    (specification->package "font-dejavu")
    (specification->package "font-fira-code")
    (specification->package "gcc-toolchain")
    (specification->package "gdb")
    (specification->package "git")
    (specification->package "git:send-email")
    (specification->package "nss-certs")
    (specification->package "nyxt")
    (specification->package "openssh")
    (specification->package "sbcl-stumpwm-cpu")
    (specification->package "sbcl-stumpwm-net")
    (specification->package "sbcl-stumpwm-pass")
    (specification->package "sbcl-stumpwm-ttf-fonts")
    (specification->package "sbcl-stumpwm-wifi")
    (specification->package "st")
    (specification->package "stumpwm-with-slynk"))
   %base-packages))

(define %thorin-services
  (append
   (list (service connman-service-type))
   (modify-services
    %desktop-services
    (delete network-manager-service-tyhpe)
    (guix-service-type config =>
                       (guix-configuration
                        (inherit config)
                        (substitute-urls
                         (append (list "https://substitutes.nonguix.org")
                                 %default-substitute-urls))
                        (authorized-keys
                         (append (list (local-file "./nonguix-key.pub"))
                                 %default-authorized-guix-keys)))))))

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Helsinki")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "thorin")
 (users (cons* (user-account
                (name "balbi")
                (comment "Felipe Balbi")
                (group "users")
                (home-directory "/home/balbi")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 (packages %thorin-packages)

 (services %thorin-services)

 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets (list "/dev/sda"))
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (swap-space
         (target
          (uuid "c32ed90c-4ff8-4893-a9cf-da1214e82dc4")))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "6bbd30d2-c1af-4559-b605-7c7d9b29606e"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))