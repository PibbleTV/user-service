//package com.pibbletv.user_service.unit;
//
//import com.pibbletv.user_service.business.ImageLoader;
//import com.pibbletv.user_service.persistance.entities.UserEntity;
//import com.pibbletv.user_service.persistance.repository.UserRepository;
//import com.pibbletv.user_service.domain.User;
//import com.pibbletv.user_service.business.implementations.UserServiceImpl;
//import com.pibbletv.user_service.business.converters.UserConverter;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.mockito.*;
//import reactor.core.publisher.Mono;
//import reactor.test.StepVerifier;
//
//import static org.mockito.Mockito.*;
//
//@ExtendWith(org.mockito.junit.jupiter.MockitoExtension.class)
//class UserServiceImplTest {
//
//
//    @Mock
//    private UserRepository userRepository;
//
//    @Mock
//    private ImageLoader imageLoader;
//
//    @InjectMocks
//    private UserServiceImpl userService;
//
//
//    @BeforeEach
//    void setup() {
//        Mockito.lenient().doAnswer(invocation -> {
//            return new byte[]{1, 2, 3};
//        }).when(imageLoader).load(anyString());
//    }
//
//    @Test
//    void testSaveUser_NewUser_SavesSuccessfully() {
//        String username = "testuser";
//
//        when(userRepository.findByUsername(username)).thenReturn(Mono.empty());
//        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.empty());
//
//        StepVerifier.create(userService.addUser(username))
//                .verifyComplete();
//
//        verify(userRepository).save(argThat(user -> user.getUsername().equals("testuser")
//        ));
//    }
//
//    @Test
//    void testUpdateUser_ExistingUser_UpdatesSuccessfully() {
//
//        UserEntity toUpdate = new UserEntity();
//        toUpdate.setId(123L);
//        toUpdate.setUsername("testuser");
//        toUpdate.setBgImage(imageLoader.load("images/default-bg.webp"));
//        toUpdate.setProfileImage(imageLoader.load("images/default-pfp.webp"));
//        toUpdate.setIsBanned(false);
//
//        User toPass = UserConverter.convertToObject(toUpdate);
//
//        when(userRepository.existsById(123L)).thenReturn(Mono.just(true));
//        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.just(new UserEntity()));
//
//        StepVerifier.create(userService.updateUser(toPass))
//                .verifyComplete();
//
//        verify(userRepository).save(argThat(user -> user.getUsername().equals("testuser")));
//    }
//
//    @Test
//    void banUser_ExistingUser_BanSuccessfully() {
//        UserEntity entity = new UserEntity();
//        entity.setId(123L);
//        entity.setUsername("testuser");
//        entity.setBgImage(imageLoader.load("images/default-bg.webp"));
//        entity.setProfileImage(imageLoader.load("images/default-pfp.webp"));
//        entity.setIsBanned(false);
//
//        when(userRepository.findByUsername("testuser")).thenReturn(Mono.just(entity));
//        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.just(entity));
//
//        StepVerifier.create(userService.banUser("testuser"))
//                .verifyComplete();
//
//        verify(userRepository).save(argThat(user ->
//                user.getUsername().equals("testuser") && Boolean.TRUE.equals(user.getIsBanned())
//        ));
//    }
//
//    @Test
//    void ubbanUser_ExistingUser_UnbanSuccessfully() {
//        UserEntity entity = new UserEntity();
//        entity.setId(123L);
//        entity.setUsername("testuser");
//        entity.setBgImage(imageLoader.load("images/default-bg.webp"));
//        entity.setProfileImage(imageLoader.load("images/default-pfp.webp"));
//        entity.setIsBanned(true);
//
//        when(userRepository.findByUsername("testuser")).thenReturn(Mono.just(entity));
//        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.just(entity));
//
//        StepVerifier.create(userService.unbanUser("testuser"))
//                .verifyComplete();
//
//        verify(userRepository).save(argThat(user ->
//                user.getUsername().equals("testuser") && Boolean.FALSE.equals(user.getIsBanned())
//        ));
//    }
//
//
//    @Test
//    void testGetUser_ExistingUser_ReturnsUser() {
//
//        String username = "testuser";
//
//        UserEntity entity = new UserEntity();
//        entity.setId(123L);
//        entity.setUsername(username);
//        entity.setBgImage(imageLoader.load("images/default-bg.webp"));
//        entity.setProfileImage(imageLoader.load("images/default-pfp.webp"));
//        entity.setIsBanned(false);
//
//        when(userRepository.findByUsername(username)).thenReturn(Mono.just(entity));
//
//        User expected = UserConverter.convertToObject(entity);
//
//        StepVerifier.create(userService.getUser(username))
//                .expectNextMatches(user -> user.getId().equals(expected.getId()) && user.getUsername().equals(expected.getUsername()))
//                .verifyComplete();
//    }
//
//    @Test
//    void testSaveUser_ExistingUser_DoesNotSaveAgain() {
//
//        String username = "testuser";
//
//        UserEntity entity = new UserEntity();
//        entity.setId(123L);
//        entity.setUsername(username);
//        entity.setBgImage(imageLoader.load("images/default-bg.webp"));
//        entity.setProfileImage(imageLoader.load("images/default-pfp.webp"));
//        entity.setIsBanned(false);
//
//        when(userRepository.findByUsername(username)).thenReturn(Mono.just(entity));
//
//        StepVerifier.create(userService.addUser(username))
//                .expectErrorMatches(throwable -> throwable instanceof RuntimeException &&
//                        throwable.getMessage().equals("User already exists"))
//                .verify();
//
//        verify(userRepository, never()).save(any());
//    }
//
//    @Test
//    void testUpdateUser_NotFound_DoesNotExist() {
//
//        UserEntity toUpdate = new UserEntity();
//        toUpdate.setId(123L);
//        toUpdate.setUsername("testuser");
//        toUpdate.setBgImage(imageLoader.load("images/default-bg.webp"));
//        toUpdate.setProfileImage(imageLoader.load("images/default-pfp.webp"));
//        toUpdate.setIsBanned(false);
//
//        User toPass = UserConverter.convertToObject(toUpdate);
//
//        when(userRepository.existsById(123L)).thenReturn(Mono.just(false));
//
//        StepVerifier.create(userService.updateUser(toPass))
//                .expectErrorMatches(throwable ->
//                        throwable instanceof IllegalArgumentException &&
//                                throwable.getMessage().equals("User does not exist"))
//                .verify();
//
//        verify(userRepository, never()).save(any());
//    }
//
//    @Test
//    void banUser_ExistingUser_UserAlreadyBanned() {
//        UserEntity entity = new UserEntity();
//        entity.setId(123L);
//        entity.setUsername("testuser");
//        entity.setBgImage(imageLoader.load("images/default-bg.webp"));
//        entity.setProfileImage(imageLoader.load("images/default-pfp.webp"));
//        entity.setIsBanned(true);
//
//        when(userRepository.findByUsername("testuser")).thenReturn(Mono.just(entity));
//
//        StepVerifier.create(userService.banUser("testuser"))
//                .expectErrorMatches(throwable ->
//                        throwable instanceof IllegalStateException &&
//                                throwable.getMessage().equals("User is already banned"))
//                .verify();
//
//        verify(userRepository, never()).save(any());
//    }
//
//    @Test
//    void ubbanUser_ExistingUser_UserIsNotBanned() {
//        UserEntity entity = new UserEntity();
//        entity.setId(123L);
//        entity.setUsername("testuser");
//        entity.setBgImage(imageLoader.load("images/default-bg.webp"));
//        entity.setProfileImage(imageLoader.load("images/default-pfp.webp"));
//        entity.setIsBanned(false);
//
//        when(userRepository.findByUsername("testuser")).thenReturn(Mono.just(entity));
//
//        StepVerifier.create(userService.unbanUser("testuser"))
//                .expectErrorMatches(throwable ->
//                        throwable instanceof IllegalStateException &&
//                                throwable.getMessage().equals("User is not banned"))
//                .verify();
//
//        verify(userRepository, never()).save(any());
//    }
//
//
//    @Test
//    void testGetUser_NotFound_ThrowsError() {
//        String username = "testuser";
//        when(userRepository.findByUsername(username)).thenReturn(Mono.empty());
//
//        StepVerifier.create(userService.getUser(username))
//                .expectErrorMatches(throwable -> throwable instanceof RuntimeException &&
//                        throwable.getMessage().equals("User not found"))
//                .verify();
//    }
//}
